type direction =
  | Up
  | Down;

type scanner = {
  position: int,
  direction,
  depth: int
};

let parseFirewall = (str) =>
  Array.map(
    (line) =>
      Utils.splitString(": ", line)
      |> (([layer, depth]) => (int_of_string(layer), int_of_string(depth))),
    Utils.linesOfString(str) |> Array.of_list
  );

let makeFirewall = (scanners) => {
  let numLayers = scanners[Array.length(scanners) - 1] |> fst |> (+)(1);
  let firewall = Array.make(numLayers, None);
  Array.iter(
    ((index, depth)) => firewall[index] = Some({position: 0, direction: Down, depth}),
    scanners
  );
  firewall
};

let moveScanner = (layer) =>
  switch layer {
  | None => layer
  | Some({position, direction, depth} as scanner) =>
    Some(
      if (position === 0 && direction == Up) {
        {...scanner, position: 1, direction: Down}
      } else if (position === depth - 1 && direction == Down) {
        {...scanner, position: depth - 2, direction: Up}
      } else {
        {...scanner, position: direction === Up ? position - 1 : position + 1}
      }
    )
  };

let calcSeverity = (packetPosition, layers) =>
  switch layers[packetPosition] {
  | Some({depth, position}) when position === 0 => Some((packetPosition, depth * packetPosition))
  | _ => None
  };

let delayFirewall = (firewall) => Array.map(moveScanner, firewall);

let crossFirewall = (~breakWhenCaught=false, firewall) => {
  let rec step = (packetPosition, firewall, severities) => {
    let severity = calcSeverity(packetPosition, firewall);
    let severities =
      switch severity {
      | Some(s) => [s, ...severities]
      | None => severities
      };
    if (packetPosition === Array.length(firewall) - 1) {
      severities
    } else if (breakWhenCaught && severity != None) {
      severities
    } else {
      step(packetPosition + 1, Array.map(moveScanner, firewall), severities)
    }
  };
  step(0, firewall, [])
};

let _ = {
  let firewallDef = parseFirewall(Inputs.day13);
  let firewall = makeFirewall(firewallDef);
  /* Part 1 */
  /* crossFirewall(firewall) |> List.fold_left((sum, severity) => sum + snd(severity), 0) |> Js.log; */
  /* Part 2 */
  let previousFirewall = ref(firewall);
  let rec findPerfectDelay = (currentDelay) => {
    let currentFirewall = delayFirewall(previousFirewall^);
    previousFirewall := currentFirewall;
    List.length(crossFirewall(~breakWhenCaught=true, currentFirewall)) === 0 ?
      currentDelay : findPerfectDelay(currentDelay + 1)
  };
  Js.log(findPerfectDelay(1))
};
