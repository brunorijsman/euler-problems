-module(spf).

-export([new_topology/0,
         new_topology/1,
         new_topology/2,
         add_link/4,
         add_bidir_link/4,
         solve/2,
         cost_root_to_point/2]).

-include_lib("eunit/include/eunit.hrl").

-record(topology, {
          points,        %% dict of Point -> {Solved, FromRootCost, FromRootPrevPoint}
          links,         %% dict of FromPoint -> [{ToPoint, Cost}]
          solved}).      %% boolean

new_topology() ->
    #topology{points = dict:new(), 
              links  = dict:new(), 
              solved = false}.

new_topology(LinkList) ->
    new_topology(LinkList, []).

new_topology(LinkList, Options) ->
    Bidir = case lists:keyfind(bidir, 1, Options) of
                {bidir, BidirValue} -> BidirValue;
                false               -> false
            end,
    Topology1 = new_topology(),
    lists:foldl(fun(L, T) -> new_topology_add_link(L, Bidir, T) end, Topology1, LinkList).

new_topology_add_link({FromPoint, ToPoint, Cost}, Bidir, Topology) ->
    case Bidir of
        true  -> add_bidir_link(Topology, FromPoint, ToPoint, Cost);
        false -> add_link(Topology, FromPoint, ToPoint, Cost)
    end.

add_link(Topology, _FromPoint, _ToPoint, _Cost) when Topology#topology.solved == true ->
    erlang:error(already_solved);

add_link(Topology, FromPoint, ToPoint, Cost) ->
    Topology1 = add_point(Topology, FromPoint),
    Topology2 = add_point(Topology1, ToPoint),
    add_link_only(Topology2, FromPoint, ToPoint, Cost).

add_bidir_link(Topology, _FromPoint, _ToPoint, _Cost) when Topology#topology.solved == true ->
    erlang:error(already_solved);

add_bidir_link(Topology, FromPoint, ToPoint, Cost) ->
    Topology1 = add_point(Topology, FromPoint),
    Topology2 = add_point(Topology1, ToPoint),
    Topology3 = add_link_only(Topology2, FromPoint, ToPoint, Cost),
    add_link_only(Topology3, ToPoint, FromPoint, Cost).

solve(Topology, _RootPoint) when Topology#topology.solved == true ->
    erlang:error(already_solved);

solve(Topology, RootPoint) ->
    Points1 = dict:store(RootPoint, {false, 0, none}, Topology#topology.points),
    Topology1 = Topology#topology{points = Points1},
    Topology2 = iterate(Topology1, RootPoint),
    Topology2#topology{solved = true}.

cost_root_to_point(Topology, _Point) when Topology#topology.solved == false ->
    erlang:error(not_solved);

cost_root_to_point(Topology, Point) ->
    {_, FromRootCost, _} = dict:fetch(Point, Topology#topology.points),
    FromRootCost.

add_point(Topology, Point) ->
    NewPoints = dict:store(Point, {false, infinite, undefined}, Topology#topology.points),
    Topology#topology{points = NewPoints}.

add_link_only(Topology, FromPoint, ToPoint, Cost) ->
    NewLinks = dict:append_list(FromPoint, [{ToPoint, Cost}], Topology#topology.links),
    Topology#topology{links = NewLinks}.

iterate(Topology, RootPoint) ->
   {Point, PointFromRootCost, Topology1} = take_unsolved_point_closest_to_root(Topology),
   case Point of
       undefined ->
           Topology1;
       _ ->
           Topology2 = relax_neighbors(Topology1, Point, PointFromRootCost),
           iterate(Topology2, RootPoint)
   end.

take_unsolved_point_closest_to_root(Topology) ->
    Closest = {undefined, infinite},    % {ClosestPoint, ClosestPointFromRootCost}
    {Point, PointFromRootCost} = dict:fold(fun closest/3, Closest, Topology#topology.points),
    Topology1 = mark_point_solved(Topology, Point),
    {Point, PointFromRootCost, Topology1}.

closest(Point, {PointSolved, PointFromRootCost, _PointFromRootPrevPoint}, Closest) ->
    {_ClosestPoint, ClosestPointFromRootCost} = Closest,
    case (not PointSolved) andalso (PointFromRootCost < ClosestPointFromRootCost) of
        true  -> {Point, PointFromRootCost};
        false -> Closest
    end.    

mark_point_solved(Topology, Point) when Point == undefined ->
    Topology;

mark_point_solved(Topology, Point) ->
    Points = Topology#topology.points,
    {_Solved, FromRootCost, FromRootPrevPoint} = dict:fetch(Point, Points),
    NewPoints = dict:store(Point, {true, FromRootCost, FromRootPrevPoint}, Points),
    Topology#topology{points = NewPoints}.
                   
relax_neighbors(Topology, _Point, PointFromRootCost) when PointFromRootCost == infinity ->
    Topology;

relax_neighbors(Topology, Point, PointFromRootCost) ->
    case dict:find(Point, Topology#topology.links) of
        {ok, Neighbors} ->
            relax_neighbors(Topology, Point, PointFromRootCost, Neighbors);
        error ->
            Topology
    end.

relax_neighbors(Topology, _Point, _PointFromRootCost, Neighbors) when Neighbors == [] ->
    Topology;

relax_neighbors(Topology, Point, PointFromRootCost, Neighbors) ->
    [Neighbor | MoreNeighbors] = Neighbors,
    NewTopology = relax_one_neighbor(Topology, Point, PointFromRootCost, Neighbor),
    relax_neighbors(NewTopology, Point, PointFromRootCost, MoreNeighbors).

relax_one_neighbor(Topology, Point, PointFromRootCost, Neighbor) ->
    {NeighborPoint, PointToNeighborCost} = Neighbor,
    {NeighborSolved, NeighborFromRootCost, _} = dict:fetch(NeighborPoint, Topology#topology.points),
    NeighborFromRootViaPointCost = sum_cost(PointFromRootCost, PointToNeighborCost),
    case NeighborFromRootViaPointCost < NeighborFromRootCost of
        true  -> 
            NewPoints = dict:store(NeighborPoint, {NeighborSolved, NeighborFromRootViaPointCost, Point}, Topology#topology.points),
            Topology#topology{points = NewPoints};
        false -> 
            Topology
    end.

sum_cost(infinite, _) -> infinite;
sum_cost(_, infinite) -> infinite;
sum_cost(X, Y)        -> X + Y. 
    
% a ----> b
%    10
%
one_uni_link_test() ->
    Topology1 = new_topology([{a, b, 10}]),
    Topology2 = solve(Topology1, a),
    ?assertEqual(0, cost_root_to_point(Topology2, a)),
    ?assertEqual(10, cost_root_to_point(Topology2, b)).
    

% a ----> b ----> c
%    10      20
%
two_uni_links_test() ->
    Topology1 = new_topology([{a, b, 10}, {b, c, 20}]),
    Topology2 = solve(Topology1, a),
    ?assertEqual(0, cost_root_to_point(Topology2, a)),
    ?assertEqual(10, cost_root_to_point(Topology2, b)),
    ?assertEqual(30, cost_root_to_point(Topology2, c)).

% a ----> b ----> c
% |  10      40   ^
% |               |
% +-----> d ------+
%    10      10
%
multi_uni_links_test() ->
    Topology1 = new_topology([{a, b, 10}, 
                              {b, c, 40},
                              {a, d, 10},
                              {d, c, 10}]),
    Topology2 = solve(Topology1, a),
    ?assertEqual(0, cost_root_to_point(Topology2, a)),
    ?assertEqual(10, cost_root_to_point(Topology2, b)),
    ?assertEqual(20, cost_root_to_point(Topology2, c)),
    ?assertEqual(10, cost_root_to_point(Topology2, d)).


% a ----> b ----> c    d ----> e
%    10      20           10
%
island_uni_links_test() ->
    Topology1 = new_topology([{a, b, 10}, 
                              {b, c, 20},
                              {d, e, 10}]),
    Topology2 = solve(Topology1, a),
    ?assertEqual(0, cost_root_to_point(Topology2, a)),
    ?assertEqual(10, cost_root_to_point(Topology2, b)),
    ?assertEqual(30, cost_root_to_point(Topology2, c)),
    ?assertEqual(infinite, cost_root_to_point(Topology2, d)),
    ?assertEqual(infinite, cost_root_to_point(Topology2, e)).
