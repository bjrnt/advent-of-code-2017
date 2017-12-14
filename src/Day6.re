open Utils;

let nextBlockForRedistribution = (memory: array(int)) =>
  List.fold_left2(((nextIndex, nextBlockCount), index, blockCount) =>
    blockCount > nextBlockCount ?
      (index, blockCount) : (nextIndex, nextBlockCount),
      (0, memory[0]), range(Array.length(memory) - 1), Array.to_list(memory)
  ) |> fst;

let redistributeBlock = (memory, blockIndex) => {
  let memLength = Array.length(memory);
  let blocksToRedistribute = memory[blockIndex];
  memory[blockIndex] = 0;
  let rec redistributeBlocks = (blocksLeft, currentBlockIndex) => {
    if (blocksLeft > 0) {
      memory[currentBlockIndex] = memory[currentBlockIndex] + 1;
      redistributeBlocks(blocksLeft - 1, (currentBlockIndex + 1) mod memLength)
    }
  };
  redistributeBlocks(blocksToRedistribute, (blockIndex + 1) mod (Array.length(memory)));
  ()
};

let redistributeMemory = (memory) => redistributeBlock(memory, nextBlockForRedistribution(memory));

/* TODO: remake this with a trie, it will be much cooler */
let redistributeUntilCycle = (memory) => {
  let memoryHistory: Hashtbl.t(list(int), int) = Hashtbl.create(100);
  let redistributions = ref(0);
  while(!Hashtbl.mem(memoryHistory, Array.to_list(memory))) {
    Hashtbl.add(memoryHistory, Array.to_list(memory), redistributions^);
    redistributeMemory(memory);
    redistributions := redistributions^ + 1;
  };
  redistributions^ - Hashtbl.find(memoryHistory, Array.to_list(memory))
};

/* let testInput = [|0,2,7,0|]; */
/* Js.log(redistributeUntilCycle(testInput)); */
Js.log(List.map(int_of_string, splitString("	", Inputs.day6)) |> Array.of_list |> redistributeUntilCycle);
