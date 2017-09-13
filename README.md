# decc

x86, x64 native code mini decompiler that decompile to pseudocode like C

## Usage
```bash
$ ./run.sh target_binary
```


```bash
$ ./run.sh examples/addMult/a.out
main() {
__x86.get_pc_thunk.ax();
local_0x10 = 0x1;
local_0xc = 0x2;
local_0x8 = (local_0xc + local_0x10);
local_0x4 = (local_0xc * local_0x8);
return 0x0;

}
$  
```

## Features

* Written in OCaml
* Simple implementation
* Using objdump as a frontend (disassembler).
