open Utils;

module IntSet =
  Set.Make(
    {
      let compare = Pervasives.compare;
      type t = int;
    }
  );

let parseNode = (str) => {
  let [node, neighbors] = splitString(" <-> ", str);
  (int_of_string(node), splitString(", ", neighbors) |> List.map(int_of_string))
};

let parseGraph = (str) => List.map(parseNode, linesOfString(str));

let makeGraph = (def) => {
  let graph = Hashtbl.create(List.length(def));
  List.iter(((node, connections)) => Hashtbl.add(graph, node, connections), def);
  graph
};

let walkGraph = (start, graph) => {
  let rec walk = (visited, queue) =>
    switch queue {
    | [x, ...xs] when ! IntSet.mem(x, visited) =>
      walk(IntSet.add(x, visited), xs @ Hashtbl.find(graph, x))
    | [x] when ! IntSet.mem(x, visited) => walk(IntSet.add(x, visited), [])
    | [_, ...xs] => walk(visited, xs);
    | [] => visited
    };
  walk(IntSet.empty, [start])
};

let _ = {
  let def = parseGraph(Inputs.day12);
  let graph = makeGraph(def);
  /* Part 1 */
  walkGraph(0, graph) |> IntSet.cardinal |> Js.log;
  /* Part 2 */
  let allNodes = List.map(fst, def) |> IntSet.of_list;
  let rec part2 = (visited, totalGroups) =>
    if (IntSet.equal(allNodes, visited)) {
      totalGroups
    } else {
      let start = IntSet.diff(allNodes, visited) |> IntSet.choose;
      let clique = walkGraph(start, graph);
      part2(IntSet.union(visited, clique), totalGroups + 1)
    };
  Js.log(part2(IntSet.empty, 0));
};
