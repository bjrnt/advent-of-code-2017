open Utils;

type tower = {
  name: string,
  weight: int,
  cumulativeWeight: option(int),
  descendants: list(string),
  parent: option(string)
};

let getParent = (tower) => tower.parent;

let decodeTowerInfo = (info: string) => {
  let name = List.hd(splitString(" ", info));
  let weight =
    splitString(")", info) |> List.hd |> splitString("(") |> List.tl |> List.hd |> int_of_string;
  let descendants =
    switch (splitString(" -> ", info)) {
    | [_, descendants] => splitString(", ", descendants)
    | _ => []
    };
  {name, weight, parent: None, cumulativeWeight: None, descendants}
};

let decodeTowerInfos = (lines: string) => linesOfString(lines) |> List.map(decodeTowerInfo);

let graph = Hashtbl.create(100);

let addNode = (node) => Hashtbl.add(graph, node.name, node);

let replaceNode = (node) => Hashtbl.replace(graph, node.name, node);

let findNode = (name) => Hashtbl.find(graph, name);

let makeGraph = (towerInfos: list(tower)) => {
  /* Init table with all nodes */
  List.iter(addNode, towerInfos);
  /* Create connections */
  List.iter(
    ({name, descendants}) =>
      List.iter(
        (descendant) => replaceNode({...findNode(descendant), parent: Some(name)}),
        descendants
      ),
    towerInfos
  );
};

/* Part 1 */
let walkUpGraph = (someNodeName: string) => {
  let rec walk = (node) =>
    switch (findNode(node) |> getParent) {
    | Some(parent) => walk(parent)
    | None => node
    };
  walk(someNodeName)
};

/* let part1 = {
     let towerInfos = decodeTowerInfos(Inputs.day7);
     let graph = makeGraph(towerInfos);
     let someNodeName = List.hd(towerInfos) |> (tower) => tower.name;
     Js.log(walkUpGraph(graph, someNodeName))
   }; */
let isListEven = ([x, ...xs]) => List.fold_left((even, next) => even && next == x, true, xs);

let findUnbalancedDisc = (root) => {
  let foundImbalance = ref(false);
  let rec findImbalance = (nodeName) => {
    let node = findNode(nodeName);
    if (List.length(node.descendants) != 0) {
      List.iter(findImbalance, node.descendants);
      let descCumWeights =
        List.map(
          (descName) => {
            let desc = findNode(descName);
            desc.cumulativeWeight
          },
          node.descendants
        );
      if (! isListEven(descCumWeights) && ! foundImbalance^) {
        Js.log("Imbalance at " ++ nodeName);
        List.iter(node => Js.log2(node.name, node.cumulativeWeight), List.map(findNode, node.descendants));
        foundImbalance := true
      }
    }
  };
  findImbalance(root)
};

let addCumulativeWeights = (root) => {
  let rec calculateCumulativeWeight = (nodeName) => {
    let node = findNode(nodeName);
    let cumulativeWeight =
      switch node.descendants {
      | [] => node.weight
      | descendants =>
        node.weight
        + List.fold_left(
            (sum, descendant) => sum + calculateCumulativeWeight(descendant),
            0,
            descendants
          )
      };
    replaceNode({...node, cumulativeWeight: Some(cumulativeWeight)});
    cumulativeWeight
  };
  let _ = calculateCumulativeWeight(root);
  ()
};

let part2 = {
  let towerInfos = decodeTowerInfos(Inputs.day7);
  makeGraph(towerInfos);
  let someNodeName = List.hd(towerInfos) |> ((tower) => tower.name);
  let root = walkUpGraph(someNodeName);
  addCumulativeWeights(root);
  findUnbalancedDisc(root)
};
