---
author: Yi-Ping Pan (Cloudlet)
date: 2025-10-15
---

# MLIR: From Tensor Graph to NPU

## Overview: MLIR Compilation Flow

**High-Level Pipeline:**

```text
┌──────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                              MLIR Compilation Pipeline                                               │
└──────────────────────────────────────────────────────────────────────────────────────────────────────┘

Input                Frontend             Middle-End            Backend               Output
─────────────────    ─────────────────    ──────────────────    ──────────────────   ─────────────────
PyTorch Model        torch-mlir/          mlir-opt              mlir-translate       NPU Binary
TensorFlow Model     tf-mlir              (Optimizations        + Backend            (model.bin)
ONNX Model           (Import)             & Lowering)           Compiler

     ↓                    ↓                     ↓                     ↓                    ↓

┌──────────┐        ┌──────────┐          ┌──────────┐          ┌──────────┐          ┌──────────┐
│  .pt     │        │  torch   │          │ linalg   │          │   NPU    │          │ model.bin│
│  .pb     │───────►│ dialect  │─────────►│ dialect  │─────────►│ dialect  │─────────►│          │
│  .onnx   │        │ (high)   │          │ (mid)    │          │ (low)    │          │ (binary) │
└──────────┘        └──────────┘          └──────────┘          └──────────┘          └──────────┘
Model File          Framework-            Hardware-             Target-              Executable
                    Specific IR           Agnostic IR           Specific IR          on NPU
```

**Detailed Step-by-Step Flow:**

```text
┌──────────────────────────────────────────────────────────────────────────────────────────────────┐
│ Step 1: Import (Frontend)                                                                        │
├──────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                  │
│  PyTorch (.pt) ──torch-mlir──► torch dialect                                                     │
│  TensorFlow (.pb) ──tf-mlir──► tf dialect                                                        │
│  ONNX (.onnx) ──onnx-mlir──► onnx dialect                                                        │
│                                                                                                  │
│  Example ops: torch.aten.conv2d, torch.aten.relu, torch.aten.matmul                              │
│  Characteristics: Framework-specific semantics, high-level operations                            │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘
                                           ↓
┌──────────────────────────────────────────────────────────────────────────────────────────────────┐
│ Step 2: High-Level Lowering (Middle-End)                                                         │
├──────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                  │
│  torch/tf dialect ──mlir-opt──► tensor/linalg dialect                                            │
│                                                                                                  │
│  Passes:                                                                                         │
│    • --convert-torch-to-linalg                                                                   │
│    • --convert-tf-to-linalg                                                                      │
│                                                                                                  │
│  Example ops: linalg.conv_2d, linalg.matmul, linalg.generic                                      │
│  Characteristics: Framework-agnostic, mathematical operations                                    │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘
                                           ↓
┌──────────────────────────────────────────────────────────────────────────────────────────────────┐
│ Step 3: Hardware-Agnostic Optimizations (Middle-End)                                             │
├──────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                  │
│  linalg dialect ──mlir-opt──► optimized linalg dialect                                           │
│                                                                                                  │
│  Optimization Passes:                                                                            │
│    • --linalg-fuse-elementwise-ops          (Fusion: conv+relu → fused_op)                       │
│    • --linalg-tile="tile-sizes=8,8"         (Tiling: better cache locality)                      │
│    • --linalg-bufferize                     (Memory management)                                  │
│    • --canonicalize                         (Simplification)                                     │
│    • --cse                                  (Common subexpression elimination)                   │
│                                                                                                  │
│  Characteristics: Generic optimizations, no hardware knowledge needed                            │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘
                                           ↓
┌──────────────────────────────────────────────────────────────────────────────────────────────────┐
│ Step 4: Hardware-Specific Lowering (Middle-End → Backend)                                        │
├──────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                  │
│  linalg dialect ──mlir-opt──► target-specific dialect                                            │
│                                                                                                  │
│  Target Selection:                                                                               │
│                                                                                                  │
│    ┌─────────────┬───────────────────────────────────────────────────────────┐                   │
│    │ Target      │ Dialect + Passes                                          │                   │
│    ├─────────────┼───────────────────────────────────────────────────────────┤                   │
│    │ NVIDIA GPU  │ gpu dialect → nvvm dialect                                │                   │
│    │             │ --convert-linalg-to-gpu → --gpu-to-nvvm                   │                   │
│    ├─────────────┼───────────────────────────────────────────────────────────┤                   │
│    │ AMD GPU     │ gpu dialect → rocdl dialect                               │                   │
│    │             │ --convert-linalg-to-gpu → --gpu-to-rocdl                  │                   │
│    ├─────────────┼───────────────────────────────────────────────────────────┤                   │
│    │ CPU (LLVM)  │ llvm dialect                                              │                   │
│    │             │ --convert-linalg-to-llvm                                  │                   │
│    ├─────────────┼───────────────────────────────────────────────────────────┤                   │
│    │ Custom NPU  │ npu dialect (YOU DEFINE)                                  │                   │
│    │             │ --convert-linalg-to-npu (YOU IMPLEMENT)                   │                   │
│    └─────────────┴───────────────────────────────────────────────────────────┘                   │
│                                                                                                  │
│  Example ops: npu.conv2d, npu.matmul, npu.dma_load                                               │
│  Characteristics: Hardware-specific operations, close to ISA                                     │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘
                                           ↓
┌──────────────────────────────────────────────────────────────────────────────────────────────────┐
│ Step 5: Target-Specific Optimizations (Backend, Optional)                                        │
├──────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                  │
│  target dialect ──mlir-opt──► optimized target dialect                                           │
│                                                                                                  │
│  Custom Optimization Passes (YOU IMPLEMENT):                                                     │
│    • --npu-fuse-conv-relu                   (Hardware-specific fusion)                           │
│    • --npu-optimize-memory-layout           (Memory hierarchy optimization)                      │
│    • --npu-schedule-operations              (Instruction scheduling)                             │
│                                                                                                  │
│  Characteristics: Hardware-specific optimizations, vendor knowledge required                     │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘
                                           ↓
┌──────────────────────────────────────────────────────────────────────────────────────────────────┐
│ Step 6: Code Emission (Backend)                                                                  │
├──────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                  │
│  target dialect ──mlir-translate + backend compiler──► binary                                    │
│                                                                                                  │
│  Emission Paths:                                                                                 │
│                                                                                                  │
│    ┌─────────────┬────────────────────────────────────────────────────────────┐                  │
│    │ Target      │ Emission Process                                            │                 │
│    ├─────────────┼────────────────────────────────────────────────────────────┤                  │
│    │ NVIDIA GPU  │ nvvm → LLVM IR → PTX → CUBIN                                │                 │
│    │             │ mlir-translate → llc → ptxas                                │                 │
│    ├─────────────┼────────────────────────────────────────────────────────────┤                  │
│    │ AMD GPU     │ rocdl → LLVM IR → AMDGPU ISA → binary                       │                 │
│    │             │ mlir-translate → llc                                        │                 │
│    ├─────────────┼────────────────────────────────────────────────────────────┤                  │
│    │ CPU         │ llvm → LLVM IR → x86/ARM assembly → object file             │                 │
│    │             │ mlir-translate → llc → assembler                            │                 │
│    ├─────────────┼────────────────────────────────────────────────────────────┤                  │
│    │ Custom NPU  │ npu → NPU ISA → model.bin (YOU IMPLEMENT)                   │                 │
│    │             │ mlir-translate --npu-to-binary                              │                 │
│    └─────────────┴────────────────────────────────────────────────────────────┘                  │
│                                                                                                  │
│  Output: model.bin (NPU instructions + weights + metadata)                                       │
│  Characteristics: Binary format, executable on target hardware                                   │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘
                                           ↓
┌──────────────────────────────────────────────────────────────────────────────────────────────────┐
│ Step 7: Runtime Execution                                                                        │
├──────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                  │
│  ┌──────────────┐                                                                                │
│  │ Application  │ (ARM/x86 CPU)                                                                  │
│  │ (C++/Python) │                                                                                │
│  └──────┬───────┘                                                                                │
│         │                                                                                        │
│         │ Loads model.bin via SDK/Driver                                                         │
│         ↓                                                                                        │
│  ┌──────────────┐                                                                                │
│  │  SDK/Driver  │ (Runtime environment)                                                          │
│  │              │ - Manages memory allocation                                                    │
│  │              │ - Schedules kernel execution                                                   │
│  │              │ - Handles data transfers                                                       │
│  └──────┬───────┘                                                                                │
│         │                                                                                        │
│         │ Dispatches instructions                                                                │
│         ↓                                                                                        │
│  ┌──────────────┐                                                                                │
│  │ NPU Hardware │                                                                                │
│  │              │ - Executes model.bin instructions                                              │
│  │              │ - Performs tensor operations                                                   │
│  │              │ - Returns results                                                              │
│  └──────────────┘                                                                                │
│                                                                                                  │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘
```

**Key Concepts Summary:**

| Stage                | Input Dialect              | Output Dialect        | Responsibility          | Who Implements                    |
| -------------------- | -------------------------- | --------------------- | ----------------------- | --------------------------------- |
| **Import**           | Model file (.pt/.pb/.onnx) | torch/tf/onnx dialect | Parse model → IR        | ✅ Provided (torch-mlir, tf-mlir) |
| **High-Level Lower** | torch/tf dialect           | linalg dialect        | Framework → generic ops | ✅ Provided (MLIR)                |
| **Generic Opts**     | linalg dialect             | optimized linalg      | Common optimizations    | ✅ Provided (MLIR)                |
| **Target Lower**     | linalg dialect             | target dialect (npu)  | Generic → hardware ops  | ⚠️ YOU implement                  |
| **Target Opts**      | npu dialect                | optimized npu         | Hardware optimizations  | ⚠️ Optional, YOU implement        |
| **Code Emission**    | npu dialect                | model.bin             | IR → binary             | ⚠️ YOU implement                  |

---

**Simplified Visual Summary:**

```text
┌─────────────────────────────────────────────────────────────────────────────┐
│                         MLIR Dialect Hierarchy                              │
└─────────────────────────────────────────────────────────────────────────────┘

                         Abstraction Level
                              ↑ High
                              │
                              │
    ┌─────────────────────────┼─────────────────────────┐
    │  Framework Dialects     │                         │
    │  (torch, tf, onnx)      │  ← Most abstract        │
    │  ✅ Provided by MLIR    │    Framework semantics  │
    └─────────────────────────┼─────────────────────────┘
                              │
                              ↓ Lower to
                              │
    ┌─────────────────────────┼─────────────────────────┐
    │  Generic ML Dialects    │                         │
    │  (tensor, linalg)       │  ← Math operations      │
    │  ✅ Provided by MLIR    │    Framework-agnostic   │
    └─────────────────────────┼─────────────────────────┘
                              │
                              ↓ Lower to
                              │
    ┌─────────────────────────┼─────────────────────────┐
    │  Hardware Dialects      │                         │
    │  (gpu, llvm, spirv)     │  ← Generic hardware     │
    │  ✅ Provided by MLIR    │    abstractions         │
    └─────────────────────────┼─────────────────────────┘
                              │
                              ↓ Lower to
                              │
    ┌─────────────────────────┼─────────────────────────┐
    │  Target-Specific        │                         │
    │  (nvvm, rocdl, npu)     │  ← Vendor-specific      │
    │  ⚠️ Custom NPU: YOU     │    Close to ISA         │
    │     implement           │                         │
    └─────────────────────────┼─────────────────────────┘
                              │
                              ↓ Low
                              │
                          Binary/ISA
```

---

## Input: Computational Graph

The input to MLIR is a **computational graph** representing the neural network model:

- **Nodes**: Operations (Conv2D, MatMul, ReLU, Softmax, etc.)
- **Edges**: Data flow between operations (tensors)
- **Graph Structure**: Represents the forward pass computation
- **Source Formats**:
  - PyTorch → TorchScript graph
  - TensorFlow → GraphDef/SavedModel
  - ONNX → ONNX graph

---

## Input: Computational Graph

The input to MLIR is a **computational graph** representing the neural network model:

- **Nodes**: Operations (Conv2D, MatMul, ReLU, Softmax, etc.)
- **Edges**: Data flow between operations (tensors)
- **Graph Structure**: Represents the forward pass computation
- **Source Formats**:
  - PyTorch → TorchScript graph
  - TensorFlow → GraphDef/SavedModel
  - ONNX → ONNX graph

### Example Computational Graph

```
Input Tensor
    ↓
  Conv2D
    ↓
  ReLU
    ↓
  MaxPool
    ↓
  Dense
    ↓
  Softmax
    ↓
Output Tensor
```

---

## Target: NPU (Neural Processing Unit)

**Definition:** Neural Processing Unit (NPU) is a specialized hardware accelerator designed to efficiently execute machine learning workloads, particularly deep learning models. NPUs are optimized for the high computational demands of neural networks, enabling faster inference and training times compared to general-purpose processors like CPUs and GPUs.

### NPU Architecture & Execution Model

**Example: MediaTek Dimensity 9400**

**NOT directly ARM binary!** The NPU has its own specialized instruction set:

```text
ARM Binary (Application) → NPU Driver/Runtime → NPU-specific Instructions → NPU Hardware
     (CPU)                   (Dispatch Layer)        (Custom ISA)           (APU/NPU)
```

- **ARM binary runs on CPU cores** - Handles application logic and orchestration
- **NPU Driver** - Translates high-level commands to NPU instructions
- **NPU ISA** - Custom instruction set optimized for tensor operations
- **Hardware execution** - NPU processes data with specialized accelerators

**Key Point**: The MLIR compilation pipeline produces NPU-specific binaries/instructions (via vendor SDK like MediaTek NeuroPilot), not standard ARM binaries.

### What's Inside model.bin?

**model.bin is NOT an ARM executable!** It's a data file containing:

```text
┌─────────────────────────────────────┐
│         model.bin Contents          │
├─────────────────────────────────────┤
│ 1. NPU Instructions (Custom ISA)    │
│    - NOT ARM instructions           │
│    - NPU-specific opcodes           │
│    - Tensor operation commands      │
│                                     │
│ 2. Model Weights (Data)             │
│    - Quantized parameters           │
│    - Convolution filters            │
│    - Bias values                    │
│                                     │
│ 3. Graph Metadata                   │
│    - Layer connections              │
│    - Tensor shapes/dimensions       │
│    - Memory layout info             │
│                                     │
│ 4. Custom Operations (Optional)     │
│    - Vendor-specific ops            │
│    - Fused operations               │
└─────────────────────────────────────┘
```

**Two Separate Binaries:**

| Binary Type              | Architecture | Runs On      | Purpose                           |
| ------------------------ | ------------ | ------------ | --------------------------------- |
| `app` (your application) | ARM ISA      | CPU cores    | Orchestration, I/O, preprocessing |
| `model.bin`              | NPU ISA      | NPU hardware | Neural network inference          |

**Analogy**: Think of `model.bin` like a GPU shader program - it's not CPU-executable, but data that gets loaded and executed by specialized hardware.

### Analogy: Java Bytecode

**YES! model.bin is very similar to Java bytecode (.class files):**

| Aspect              | Java Bytecode (.class)        | model.bin (NPU)            |
| ------------------- | ----------------------------- | -------------------------- |
| **Type**            | Platform-independent bytecode | Hardware-specific bytecode |
| **Self-executing?** | ❌ No                         | ❌ No                      |
| **Needs runtime**   | ✅ JVM (Java Virtual Machine) | ✅ SDK/Driver + NPU        |
| **Contains**        | JVM instructions + data       | NPU instructions + weights |
| **Executed by**     | JVM interpreter/JIT           | NPU hardware               |
| **Host program**    | `java` launcher               | Your app (ARM binary)      |

**Side-by-side comparison:**

```text
Java:                           NPU:
┌─────────────┐                ┌─────────────┐
│ MyApp.class │                │ model.bin   │
│ (bytecode)  │                │ (NPU ISA)   │
└──────┬──────┘                └──────┬──────┘
       │ loaded by                    │ loaded by
       ↓                              ↓
┌─────────────┐                ┌─────────────┐
│     JVM     │                │  SDK/Driver │
│ (runtime)   │                │  (runtime)  │
└──────┬──────┘                └──────┬──────┘
       │ executes on                  │ executes on
       ↓                              ↓
┌─────────────┐                ┌─────────────┐
│  CPU cores  │                │ NPU hardware│
└─────────────┘                └─────────────┘
```

**Key insight:** Just like `.class` files need JVM to run, `model.bin` needs the SDK/driver runtime to execute on NPU!

---

## Compiler Architecture: T-Diagram Representation

So the ML compiler part's T-diagram is:

```text
  PyTorch           MLIR IR             NPU ISA
  TorchScript      (High-Level)       (model.bin)
     ┌──────┐        ┌──────┐           ┌──────┐
     │      │        │      │           │      │
     │ FE   │   →    │ Mid  │     →     │ BE   │
     │      │        │      │           │      │
     └───┬──┘        └───┬──┘           └───┬──┘
         │               │                  │
      Python          C++/LLVM           C++/LLVM
     (Importer)    (MLIR Core)        (Target Backend)

```

---

## MLIR Compilation Pipeline

### Common Questions

**Q: After using torch-mlir/tf-mlir-translate to convert to MLIR, what's next?**

- **A.** What is next step?
- **B.** Do we need to define our target dialect? Or llvm backend will take care if we defined iSel or RA?
- **C.** Is this compiler frontend to middle-end jobs?

### Answer C: MLIR's Role Across Compiler Stages

**MLIR encompasses all three compiler stages:**

**Traditional Compiler Structure:**

```text
Frontend → Middle-end → Backend
(parsing)  (optimization) (code gen)
```

**MLIR's Scope:**

| Stage          | Traditional Compiler      | MLIR ML Compiler              | What Happens                         |
| -------------- | ------------------------- | ----------------------------- | ------------------------------------ |
| **Frontend**   | Lexing, parsing, AST      | torch-mlir, tf-mlir-translate | Import model → MLIR dialects         |
| **Middle-end** | IR optimization, analysis | mlir-opt passes               | Dialect lowering, graph optimization |
| **Backend**    | Code generation, iSel, RA | mlir-translate + backend      | Generate target binary/ISA           |

**MLIR is primarily middle-end but provides infrastructure for all stages:**

**Key Distinction:**

- **Using LLVM backend path**: MLIR does frontend + middle-end, LLVM does backend (iSel, RA)
- **Custom NPU path**: MLIR does frontend + middle-end, YOU do backend

**Answer Summary:**

| Question                      | Answer                                             |
| ----------------------------- | -------------------------------------------------- |
| Is torch-mlir frontend work?  | ✅ Yes - imports model to MLIR IR                  |
| Is mlir-opt middle-end work?  | ✅ Yes - optimizes and lowers dialects             |
| Is MLIR only frontend+middle? | ❌ No - it can do backend too (via LLVM or custom) |

### Answer A & B: Two Compilation Paths

**It depends on your target!**

#### Path 1: CPU/GPU via LLVM (No custom dialect needed)

```text
Step 1: Use Existing Optimizations (FREE!)
───────────────────────────────────────────
$ mlir-opt model.mlir \
    --convert-torch-to-linalg \           ← Provided
    --linalg-fuse-elementwise-ops \       ← Provided (fusion opt)
    --linalg-tile="tile-sizes=8,8" \      ← Provided (tiling opt)
    --convert-linalg-to-affine \          ← Provided
    --affine-loop-fusion \                ← Provided (loop opt)
    -o model_optimized.mlir

✅ You DON'T write these optimizations - they're already in MLIR!

Step 2: Custom Lowering (YOU IMPLEMENT)
────────────────────────────────────────
$ mlir-opt model_optimized.mlir \
    --convert-linalg-to-npu \             ← YOU write this pass
    -o model_npu.mlir

You implement conversion patterns:
┌─────────────────────────────────────────┐
│ linalg.conv_2d → npu.conv2d             │
│ linalg.matmul → npu.matmul              │
│ tensor.extract_slice → npu.dma_load     │
└─────────────────────────────────────────┘

Step 3: Optional Target-Specific Opts (YOUR CHOICE)
────────────────────────────────────────────────────
$ mlir-opt model_npu.mlir \
    --npu-fuse-conv-relu \                ← Optional: YOU write this
    --npu-optimize-memory-layout \        ← Optional: YOU write this
    -o model_npu_opt.mlir

Examples of custom optimizations:
• Fuse NPU ops for hardware efficiency
• Optimize for NPU memory hierarchy
• Schedule ops for maximum throughput

Step 4: Code Emission (YOU IMPLEMENT)
──────────────────────────────────────
$ mlir-translate model_npu_opt.mlir \
    --npu-to-binary \                     ← YOU write this translator
    -o model.bin

1-to-1 mapping:
┌──────────────────────────────────────────┐
│ npu.conv2d %0, %1                        │
│   ↓ translates to                        │
│ 0x01A4: CONV2D_OP r0, r1, [config]       │ ← Binary instruction
│                                          │
│ npu.matmul %2, %3                        │
│   ↓ translates to                        │
│ 0x01B8: MATMUL_OP r2, r3, [config]       │ ← Binary instruction
└──────────────────────────────────────────┘
```

## Example: What You Actually Code

### 1. Dialect Lowering (REQUIRED)

```cpp
// YOU implement this conversion pattern
class ConvertLinalgConvToNPU : public OpRewritePattern<linalg::Conv2DOp> {
  LogicalResult matchAndRewrite(linalg::Conv2DOp op,
                                 PatternRewriter &rewriter) const override {
    // Convert linalg.conv_2d → npu.conv2d
    rewriter.replaceOpWithNewOp<npu::Conv2DOp>(
        op, op.getInput(), op.getFilter(),
        /*npu_specific_attrs*/);
    return success();
  }
};
```

### 2. Optional Custom Optimization (YOUR CHOICE)

```cpp
// Optional: Fuse conv+relu for better NPU performance
class FuseNPUConvRelu : public OpRewritePattern<npu::ReluOp> {
  LogicalResult matchAndRewrite(npu::ReluOp relu,
                                 PatternRewriter &rewriter) const override {
    auto conv = relu.getInput().getDefiningOp<npu::Conv2DOp>();
    if (!conv) return failure();

    // Replace with fused op
    rewriter.replaceOpWithNewOp<npu::FusedConvReluOp>(relu, conv.getInput(), ...);
    return success();
  }
};
```

### 3. Code Emission (REQUIRED)

```cpp
// YOU implement binary emission
void emitNPUConv2D(npu::Conv2DOp op, BinaryEmitter &emitter) {
  // Emit NPU instruction opcode
  emitter.emitOpcode(NPU_CONV2D_OP);

  // Emit operands
  emitter.emitRegister(op.getInput());
  emitter.emitRegister(op.getFilter());

  // Emit configuration
  emitter.emitConfig(op.getStride(), op.getPadding());
}
```

---

## Multi-Target Strategy: Supporting Multiple NPU Architectures

### The Problem: Multiple Target Architectures

**MLIR Lowering Definition:**

> Lowering = Transforming a higher-level representation of an operation into a lower-level, but **semantically equivalent**, representation.

**Company Scenario:**

Your company needs to compile ML models for different NPU architectures:

```text
High-Level (Semantic):
linalg.conv_2d, linalg.matmul, etc.  ← Same for all targets

         ↓ [LOWERING]

Target-Specific (ISA):
NPU A Dialect:                  NPU B Dialect:
- npu_a.conv2d                  - npu_b.conv2d
- npu_a.matmul                  - npu_b.matmul
- npu_a.special_op_c            - npu_b.special_op_d
                                - npu_b.special_op_f
```

**Key Insight:**

- Operations like `conv2d` have the **same semantics** on both NPUs (what they compute)
- But they have **different target representations** (how they're encoded in ISA)
- Some NPUs support unique operations (e.g., `special_op_c` only on A)

**Challenge:** How to modularize the compiler to:

1. Share common high-level optimizations across all targets
2. Define separate **conversion targets** for each NPU
3. Support multiple targets without code duplication

### 5.2 Solution: Modularization Strategy

#### Option 1: Separate Dialects per Target (Most Modular)

```text
┌─────────────────────────────────────────────────────────────┐
│            Shared High-Level Dialects (Common)              │
│                                                             │
│  torch/tf dialect → tensor/linalg dialect                   │
│  (Illegal after lowering - must be converted)               │
│                                                             │
└──────────────────────┬──────────────────────────────────────┘
                       │
          ┌────────────┴────────────┐
          │     [LOWERING]          │
          ↓                         ↓
┌─────────────────────┐   ┌─────────────────────┐
│   NPU_A Dialect     │   │   NPU_B Dialect     │
│   (Legal target)    │   │   (Legal target)    │
│                     │   │                     │
│   • npu_a.conv2d    │   │   • npu_b.conv2d    │
│   • npu_a.matmul    │   │   • npu_b.matmul    │
│   • npu_a.special_c │   │   • npu_b.special_d │
│                     │   │   • npu_b.special_f │
└──────────┬──────────┘   └──────────┬──────────┘
           │                         │
           ↓                         ↓
┌─────────────────────┐   ┌─────────────────────┐
│  NPU_A Backend      │   │  NPU_B Backend      │
│  (codegen)          │   │  (codegen)          │
└─────────────────────┘   └─────────────────────┘
```

**Code Structure:**

```text
lib/
├── Dialect/
│   ├── NPU_A/
│   │   ├── IR/
│   │   │   ├── NPU_A_Ops.td          # Define conv2d, matmul, special_c
│   │   │   └── NPU_A_Dialect.cpp
│   │   ├── Transforms/
│   │   │   └── LinalgToNPU_A.cpp     # Lowering passes
│   │   └── Target/
│   │       └── NPU_A_Binary.cpp      # Code emission
│   │
│   └── NPU_B/
│       ├── IR/
│       │   ├── NPU_B_Ops.td          # Define conv2d, matmul, special_d, special_f
│       │   └── NPU_B_Dialect.cpp
│       ├── Transforms/
│       │   └── LinalgToNPU_B.cpp     # Lowering passes
│       └── Target/
│           └── NPU_B_Binary.cpp      # Code emission
│
└── Common/                            # Shared utilities
    └── NPUCommon.cpp                  # Common base classes
```

#### Option 2: Single Dialect with Target-Specific Attributes (Less Modular)

```text
┌─────────────────────────────────────────┐
│         Unified NPU Dialect             │
│                                         │
│  npu.conv2d() [target=A|B]              │
│  npu.matmul() [target=A|B]              │
│  npu.special_c() [target=A]     ← A-only│
│  npu.special_d() [target=B]     ← B-only│
│  npu.special_f() [target=B]     ← B-only│
│                                         │
└──────────────┬──────────────────────────┘
               │
    ┌──────────┴──────────┐
    │                     │
    ↓                     ↓
┌──────────────┐    ┌──────────────┐
│ NPU_A Backend│    │ NPU_B Backend│
└──────────────┘    └──────────────┘
```

**Less recommended** - harder to maintain, more runtime checks.

#### Option 3: Hybrid Approach (Recommended for Companies)

```text
┌──────────────────────────────────────────────────────────┐
│          Shared High-Level (torch/tf/linalg)             │
└────────────────────────┬─────────────────────────────────┘
                         │
┌────────────────────────┴─────────────────────────────────┐
│         Common NPU Abstraction Layer (Optional)          │
│                                                          │
│  Generic ops that exist on multiple targets:             │
│  • npu_common.conv2d()  ← Maps to A::conv or B::conv     │
│  • npu_common.matmul()  ← Maps to A::matmul or B::matmul │
│                                                          │
└────────────────────────┬─────────────────────────────────┘
                         │
          ┌──────────────┴──────────────┐
          │                             │
          ↓                             ↓
┌─────────────────────┐       ┌─────────────────────┐
│  Target-Specific    │       │  Target-Specific    │
│  NPU_A Dialect      │       │  NPU_B Dialect      │
│                     │       │                     │
│  • Unique A ops     │       │  • Unique B ops     │
│  • A-specific attrs │       │  • B-specific attrs │
└─────────────────────┘       └─────────────────────┘
```

### 5.3 Recommended Modularization Points

#### Stage 1: Common Frontend & Middle-End

```text
Responsibilities: SHARED across all NPU targets

torch/tf dialect
    ↓
[Common Optimizations]
    • Constant folding
    • Dead code elimination
    ↓
tensor/linalg dialect
    ↓
[Common ML Optimizations]
    • Op fusion
    • Tiling
    • Loop optimization
    ↓
Common IR (linalg/affine)
```

**Code:** Shared library used by all targets.

#### Stage 2: Target Selection & Lowering

```text
Responsibilities: TARGET-SPECIFIC

       Common IR
           ↓
   [Target Selection]
           ↓
    ┌──────┴──────┐
    ↓             ↓
[NPU_A Lowering]  [NPU_B Lowering]
    ↓             ↓
npu_a.conv2d  npu_b.conv2d
npu_a.matmul  npu_b.matmul
npu_a.special_c  npu_b.special_d
              npu_b.special_f
```

**Code:** Separate lowering passes per target.

```cpp
// File: lib/Dialect/NPU_A/Transforms/LinalgToNPU_A.cpp
void populateLinalgToNPU_A_Patterns(RewritePatternSet &patterns) {
  patterns.add<ConvertConv2DToNPU_A>(patterns.getContext());
  patterns.add<ConvertMatMulToNPU_A>(patterns.getContext());
  patterns.add<ConvertSpecialOpToNPU_A_C>(patterns.getContext());
}

// File: lib/Dialect/NPU_B/Transforms/LinalgToNPU_B.cpp
void populateLinalgToNPU_B_Patterns(RewritePatternSet &patterns) {
  patterns.add<ConvertConv2DToNPU_B>(patterns.getContext());
  patterns.add<ConvertMatMulToNPU_B>(patterns.getContext());
  patterns.add<ConvertOp1ToNPU_B_D>(patterns.getContext());
  patterns.add<ConvertOp2ToNPU_B_F>(patterns.getContext());
}
```

#### Stage 3: Target-Specific Optimization (Optional)

```text
Responsibilities: TARGET-SPECIFIC

npu_a dialect        npu_b dialect
    ↓                    ↓
[NPU_A Opts]        [NPU_B Opts]
    ↓                    ↓
optimized            optimized
```

**Code:** Optional target-specific passes.

#### Stage 4: Code Emission

```text
Responsibilities: TARGET-SPECIFIC

npu_a dialect        npu_b dialect
    ↓                    ↓
[NPU_A Codegen]     [NPU_B Codegen]
    ↓                    ↓
model_a.bin         model_b.bin
```

**Code:** Completely separate backends.

```cpp
// File: lib/Target/NPU_A/BinaryEmitter.cpp
void emitNPU_A_Binary(Module &module, raw_ostream &os) {
  for (auto op : module.getOps<npu_a::Conv2DOp>()) {
    os << encodeConv2DOp(op);
  }
  for (auto op : module.getOps<npu_a::MatMulOp>()) {
    os << encodeMatMulOp(op);
  }
  for (auto op : module.getOps<npu_a::SpecialCOp>()) {
    os << encodeSpecialCOp(op);
  }
}

// File: lib/Target/NPU_B/BinaryEmitter.cpp
void emitNPU_B_Binary(Module &module, raw_ostream &os) {
  for (auto op : module.getOps<npu_b::Conv2DOp>()) {
    os << encodeConv2DOp(op);
  }
  for (auto op : module.getOps<npu_b::MatMulOp>()) {
    os << encodeMatMulOp(op);
  }
  for (auto op : module.getOps<npu_b::SpecialDOp>()) {
    os << encodeSpecialDOp(op);
  }
  for (auto op : module.getOps<npu_b::SpecialFOp>()) {
    os << encodeSpecialFOp(op);
  }
}
```

### 5.4 Compilation Flow Example

```bash
# Common frontend (same for both targets)
$ torch-mlir model.pt -o model.mlir

# Common optimizations (same for both targets)
$ mlir-opt model.mlir \
    --convert-torch-to-linalg \
    --linalg-fuse-elementwise-ops \
    -o model_opt.mlir

# Target selection and lowering (DIFFERENT)
# For NPU A:
$ mlir-opt model_opt.mlir \
    --convert-linalg-to-npu-a \
    --npu-a-optimize \
    -o model_npu_a.mlir

# For NPU B:
$ mlir-opt model_opt.mlir \
    --convert-linalg-to-npu-b \
    --npu-b-optimize \
    -o model_npu_b.mlir

# Code generation (DIFFERENT)
$ mlir-translate model_npu_a.mlir --npu-a-to-binary -o model_a.bin
$ mlir-translate model_npu_b.mlir --npu-b-to-binary -o model_b.bin
```

### 5.5 Build System Structure

```cmake
# CMakeLists.txt structure
add_subdirectory(Common)          # Shared utilities
add_subdirectory(Dialect/NPU_A)   # NPU A specific
add_subdirectory(Dialect/NPU_B)   # NPU B specific

add_library(MLIRNPUCommon STATIC
  Common/NPUBase.cpp
  Common/NPUUtils.cpp
)

add_library(MLIRNPU_A STATIC
  Dialect/NPU_A/IR/NPU_A_Ops.cpp
  Dialect/NPU_A/Transforms/LinalgToNPU_A.cpp
  Dialect/NPU_A/Target/NPU_A_Binary.cpp
)
target_link_libraries(MLIRNPU_A PRIVATE MLIRNPUCommon)

add_library(MLIRNPU_B STATIC
  Dialect/NPU_B/IR/NPU_B_Ops.cpp
  Dialect/NPU_B/Transforms/LinalgToNPU_B.cpp
  Dialect/NPU_B/Target/NPU_B_Binary.cpp
)
target_link_libraries(MLIRNPU_B PRIVATE MLIRNPUCommon)
```

### Answer Summary: Where to Modularize?

| Stage                                       | Modularization Strategy    | Why                             |
| ------------------------------------------- | -------------------------- | ------------------------------- |
| **Frontend** (torch-mlir/tf-mlir)           | ✅ **SHARED**              | Same for all targets            |
| **High-level opts** (linalg fusion, tiling) | ✅ **SHARED**              | Hardware-agnostic optimizations |
| **Dialect conversion** (linalg → npu)       | ⚠️ **SEPARATE per target** | Different op mappings           |
| **Target dialect** (npu_a vs npu_b)         | ⚠️ **SEPARATE per target** | Different operation sets        |
| **Target-specific opts**                    | ⚠️ **SEPARATE per target** | Hardware-specific optimizations |
| **Code emission**                           | ⚠️ **SEPARATE per target** | Different ISAs/binary formats   |

**Key Principle:**

- **Modularize at the dialect level** (after linalg, before target-specific lowering)
- **Share everything before target-specific ops**
- **Separate everything after target selection**

This gives you:

- ✅ Maximum code reuse (common frontend/middle-end)
- ✅ Clean separation of concerns
- ✅ Easy to add new NPU targets (NPU C, NPU D, etc.)
- ✅ Independent development of target backends

---

## Lowering 實作詳解：Pattern Rewriting vs Instruction Selection

### 核心概念：Lowering 不是 1-1 Mapping

**❌ 錯誤理解：**

> "Higher dialect to lower dialect 就是一個 1-1 的 mapping"

**✅ 正確理解：**

> Lowering 是一個**語意保持的轉換過程**，將高層操作轉換為低層的等價表示。這個過程**不一定是 1-1 mapping**。

#### Lowering 的多種映射關係

```text
┌────────────────────────────────────────────────────────────┐
│ Lowering = Semantic-preserving transformation              │
│                                                            │
│ Mapping types:                                             │
│ • 1 → 1  (simple conversion)                               │
│ • 1 → N  (decomposition)                                   │
│ • N → 1  (fusion)                                          │
│ • N → M  (reorganization)                                  │
│ • 1 → 0  (elimination)                                     │
│                                                            │
│ Key property: SEMANTICS stay the same                      │
│ (output is mathematically equivalent)                      │
└────────────────────────────────────────────────────────────┘
```

#### 實際例子比較

| Scenario  | Higher Dialect        | Lower Dialect            | Mapping Type |
| --------- | --------------------- | ------------------------ | ------------ |
| 簡單轉換  | `linalg.matmul`       | `npu.matmul`             | ✅ 1→1       |
| Conv 分解 | `linalg.conv_2d`      | `im2col + gemm + col2im` | ❌ 1→3       |
| 融合優化  | `conv + relu + pool`  | `npu.fused_block`        | ❌ 3→1       |
| Batch ops | `linalg.batch_matmul` | `loop { matmul }`        | ❌ 1→N       |
| Identity  | `linalg.identity`     | `(removed by DCE)`       | ❌ 1→0       |

---

### MLIR vs LLVM: 為什麼不需要 Cost Model？

#### LLVM iSel 的方式（需要 Cost Model）

```cpp
// LLVM TableGen: Instruction Selection with Cost
def : Pat<(add GPR:$a, GPR:$b),
          (ADD_RR $a, $b)>;                    // Cost = 1

def : Pat<(add GPR:$a, imm:$b),
          (ADD_RI $a, $b)>;                    // Cost = 1

def : Pat<(add (load addr:$a), GPR:$b),
          (ADD_RM $a, $b)>;                    // Cost = 2 (mem operand)

// Problem: 一個 add 操作有多種可能的指令組合
// Solution: 計算每個 pattern 的 cost，選最小的
```

**LLVM iSel 需要 Cost Model 的原因：**

- 多對多映射：一個 IR op 可以對應多種 ISA 指令組合
- 需要選擇：哪種指令序列執行最快？寄存器壓力最小？
- Cost function：考慮執行時間、register pressure、code size 等

#### MLIR Lowering 的方式（無需 Cost Model）

```cpp
// MLIR Pattern Rewriting: Declarative Transformation
patterns.add<ConvertLinalgMatMulToNPU_A>(context);  // 直接註冊 pattern
patterns.add<ConvertLinalgConv2DToNPU_A>(context);
patterns.add<FuseConvReluForNPU_A>(context);

// Pattern 會自動 match 和 apply，不需要 cost 比較
// 每個 pattern 定義明確的語意轉換規則
```

**MLIR 不需要 Cost Model 的原因：**

| LLVM iSel 的情況                                    | MLIR Lowering 的情況                             |
| --------------------------------------------------- | ------------------------------------------------ |
| **多對多映射**：`add` 可以用 ADD_RR, ADD_RI, ADD_RM | **確定性映射**：`linalg.matmul` → `npu_a.matmul` |
| **需要選擇**：哪種指令組合最優？                    | **無需選擇**：明確的語意對應                     |
| **Cost function**：執行時間、register pressure      | **無 cost**：只要語意正確即可                    |
| **DAG matching**：需要搜尋最佳匹配                  | **Pattern matching**：直接套用規則               |

---

### Lowering 實作方式 1: Simple 1-to-1 Conversion

```cpp
// 場景：Linalg dialect → NPU_A dialect
// Linalg ops: {conv_2d, matmul, add, relu}
// NPU_A ops:  {conv2d, matmul, special_fused_op, dma_load}

// ============================================
// Pattern 1: Simple 1-to-1 Conversion
// linalg.matmul → npu_a.matmul
// ============================================
class ConvertLinalgMatMulToNPU_A : public OpRewritePattern<linalg::MatmulOp> {
public:
  using OpRewritePattern<linalg::MatmulOp>::OpRewritePattern;

  LogicalResult matchAndRewrite(linalg::MatmulOp op,
                                 PatternRewriter &rewriter) const override {
    // Extract operands from the linalg operation
    Value lhs = op.getInputs()[0];
    Value rhs = op.getInputs()[1];
    Value out = op.getOutputs()[0];

    // Create NPU_A matmul op (1-to-1 mapping)
    rewriter.replaceOpWithNewOp<npu_a::MatMulOp>(
        op,                          // Replace this op
        out.getType(),               // Result type
        lhs, rhs,                    // Operands
        rewriter.getI32IntegerAttr(0) // NPU config (tile size, etc.)
    );

    return success();
  }
};
```

**Transformation Example:**

```mlir
// Before lowering (Linalg dialect)
%result = linalg.matmul ins(%A, %B : tensor<128x256xf32>, tensor<256x512xf32>)
                        outs(%C : tensor<128x512xf32>) -> tensor<128x512xf32>

// After lowering (NPU_A dialect)
%result = npu_a.matmul %A, %B {tile_size = 0 : i32} :
          (tensor<128x256xf32>, tensor<256x512xf32>) -> tensor<128x512xf32>
```

**Mapping Type:** ✅ **1 → 1** (simple conversion)

---

### Lowering 實作方式 2: 1-to-N Decomposition

```cpp
// ============================================
// Pattern 2: 1-to-N Decomposition
// linalg.conv_2d → npu_a.dma_load + npu_a.conv2d + npu_a.dma_store
// ============================================
class ConvertLinalgConv2DToNPU_A : public OpRewritePattern<linalg::Conv2DNhwcHwcfOp> {
public:
  using OpRewritePattern::OpRewritePattern;

  LogicalResult matchAndRewrite(linalg::Conv2DNhwcHwcfOp op,
                                 PatternRewriter &rewriter) const override {
    Location loc = op.getLoc();
    Value input = op.getInputs()[0];
    Value filter = op.getInputs()[1];
    Value output = op.getOutputs()[0];

    // Step 1: DMA load input to NPU local memory
    auto inputLocal = rewriter.create<npu_a::DMALoadOp>(
        loc, input.getType(), input,
        rewriter.getI32IntegerAttr(NPU_LOCAL_MEM)
    );

    // Step 2: DMA load filter to NPU local memory
    auto filterLocal = rewriter.create<npu_a::DMALoadOp>(
        loc, filter.getType(), filter,
        rewriter.getI32IntegerAttr(NPU_LOCAL_MEM)
    );

    // Step 3: Perform convolution on NPU
    auto convResult = rewriter.create<npu_a::Conv2DOp>(
        loc, output.getType(),
        inputLocal.getResult(),
        filterLocal.getResult(),
        op.getStrides(),
        op.getDilations(),
        op.getPadding()
    );

    // Step 4: DMA store result back to global memory
    rewriter.replaceOpWithNewOp<npu_a::DMAStoreOp>(
        op,
        convResult.getResult(),
        output,
        rewriter.getI32IntegerAttr(GLOBAL_MEM)
    );

    // Result: 1 high-level op → 4 low-level ops
    return success();
  }
};
```

**Transformation Example:**

```mlir
// Before lowering (Linalg dialect) - 1 operation
%result = linalg.conv_2d_nhwc_hwcf
  ins(%input, %filter : tensor<1x224x224x3xf32>, tensor<7x7x3x64xf32>)
  outs(%output : tensor<1x112x112x64xf32>) -> tensor<1x112x112x64xf32>

// After lowering (NPU_A dialect) - 4 operations
%input_local = npu_a.dma_load %input {mem_space = 1 : i32}
  : tensor<1x224x224x3xf32> -> tensor<1x224x224x3xf32>

%filter_local = npu_a.dma_load %filter {mem_space = 1 : i32}
  : tensor<7x7x3x64xf32> -> tensor<7x7x3x64xf32>

%conv_result = npu_a.conv2d %input_local, %filter_local
  {strides = [2, 2], padding = [3, 3, 3, 3]}
  : (tensor<1x224x224x3xf32>, tensor<7x7x3x64xf32>) -> tensor<1x112x112x64xf32>

%result = npu_a.dma_store %conv_result, %output {mem_space = 0 : i32}
  : tensor<1x112x112x64xf32>
```

**Mapping Type:** ❌ **1 → N** (decomposition)

**Why decompose?**

- NPU 需要 explicit memory management (DMA transfers)
- 區分 global memory vs local memory operations
- 更貼近硬件的執行模型

---

### Lowering 實作方式 3: N-to-1 Fusion

```cpp
// ============================================
// Pattern 3: N-to-1 Fusion
// linalg.conv_2d + linalg.relu → npu_a.fused_conv_relu
// ============================================
class FuseConvReluForNPU_A : public OpRewritePattern<linalg::GenericOp> {
public:
  using OpRewritePattern::OpRewritePattern;

  LogicalResult matchAndRewrite(linalg::GenericOp reluOp,
                                 PatternRewriter &rewriter) const override {
    // Check if this is a relu operation
    if (!isReLUOp(reluOp))
      return failure();

    // Check if input comes from conv2d
    auto convOp = reluOp.getInputs()[0].getDefiningOp<linalg::Conv2DNhwcHwcfOp>();
    if (!convOp)
      return failure();

    // Check if conv has only one use (safe to fuse)
    if (!convOp.getResult(0).hasOneUse())
      return failure();

    // Replace both ops with fused version
    rewriter.replaceOpWithNewOp<npu_a::FusedConvReluOp>(
        reluOp,                      // Replace relu op
        reluOp.getResult(0).getType(),
        convOp.getInputs()[0],       // Input from conv
        convOp.getInputs()[1],       // Filter from conv
        convOp.getStrides(),
        convOp.getDilations()
    );

    // Erase the conv op (since it's fused)
    rewriter.eraseOp(convOp);

    // Result: 2 high-level ops → 1 fused low-level op
    return success();
  }

private:
  bool isReLUOp(linalg::GenericOp op) const {
    // Check if the body performs max(x, 0)
    // Implementation details omitted for brevity
    return true;
  }
};
```

**Transformation Example:**

```mlir
// Before lowering (Linalg dialect) - 2 operations
%conv_result = linalg.conv_2d_nhwc_hwcf
  ins(%input, %filter : tensor<1x224x224x3xf32>, tensor<3x3x3x64xf32>)
  outs(%temp : tensor<1x224x224x64xf32>) -> tensor<1x224x224x64xf32>

%relu_result = linalg.generic {
  indexing_maps = [affine_map<(d0, d1, d2, d3) -> (d0, d1, d2, d3)>],
  iterator_types = ["parallel", "parallel", "parallel", "parallel"]
} ins(%conv_result : tensor<1x224x224x64xf32>)
  outs(%output : tensor<1x224x224x64xf32>) {
^bb0(%in: f32, %out: f32):
  %zero = arith.constant 0.0 : f32
  %max = arith.maxf %in, %zero : f32
  linalg.yield %max : f32
} -> tensor<1x224x224x64xf32>

// After lowering (NPU_A dialect) - 1 fused operation
%result = npu_a.fused_conv_relu %input, %filter
  {strides = [1, 1], dilations = [1, 1]}
  : (tensor<1x224x224x3xf32>, tensor<3x3x3x64xf32>) -> tensor<1x224x224x64xf32>
```

**Mapping Type:** ❌ **N → 1** (fusion)

**Why fuse?**

- 減少 memory traffic (不需要寫回 intermediate result)
- 更好的 data locality
- NPU 硬件可能原生支援 fused operations

---

### 完整的 Lowering Pass 實作

```cpp
// File: lib/Dialect/NPU_A/Transforms/LinalgToNPU_A.cpp

#include "mlir/Dialect/Linalg/IR/Linalg.h"
#include "mlir/Transforms/DialectConversion.h"
#include "NPU_A/IR/NPU_A_Ops.h"

namespace {

// ============================================
// Step 1: Define all conversion patterns
// ============================================
void populateLinalgToNPU_A_Patterns(RewritePatternSet &patterns) {
  patterns.add<
      ConvertLinalgMatMulToNPU_A,      // 1→1 conversion
      ConvertLinalgConv2DToNPU_A,      // 1→N decomposition
      ConvertLinalgAddToNPU_A,         // 1→1 conversion
      FuseConvReluForNPU_A             // N→1 fusion
  >(patterns.getContext());
}

// ============================================
// Step 2: Define conversion target
// (這裡定義什麼是 legal，什麼是 illegal)
// ============================================
class LinalgToNPU_A_Pass : public PassWrapper<LinalgToNPU_A_Pass,
                                               OperationPass<ModuleOp>> {
public:
  void runOnOperation() override {
    MLIRContext *context = &getContext();
    RewritePatternSet patterns(context);
    ConversionTarget target(*context);

    // ===== Define what's LEGAL after lowering =====
    target.addLegalDialect<npu_a::NPU_A_Dialect>();  // NPU_A ops are legal
    target.addLegalDialect<func::FuncDialect>();     // Keep function structure
    target.addLegalDialect<arith::ArithDialect>();   // Keep arithmetic ops

    // ===== Define what's ILLEGAL after lowering =====
    target.addIllegalDialect<linalg::LinalgDialect>(); // Must convert all linalg ops
    target.addIllegalDialect<tensor::TensorDialect>(); // Must convert all tensor ops

    // ===== Optional: Conditionally legal ops =====
    target.addDynamicallyLegalOp<linalg::FillOp>([](linalg::FillOp op) {
      // Allow fill ops if they only fill with zero
      return isFillZero(op);
    });

    // ===== Register conversion patterns =====
    populateLinalgToNPU_A_Patterns(patterns);

    // ===== Apply conversion =====
    // This will fail if any illegal op remains after conversion
    if (failed(applyPartialConversion(getOperation(), target, std::move(patterns)))) {
      signalPassFailure();
    }
  }
};

} // namespace

// ============================================
// Step 3: Register the pass
// ============================================
std::unique_ptr<Pass> createLinalgToNPU_A_Pass() {
  return std::make_unique<LinalgToNPU_A_Pass>();
}
```

**Key Components:**

1. **RewritePatternSet**: 收集所有的 conversion patterns
2. **ConversionTarget**: 定義 legal/illegal operations
3. **applyPartialConversion**: 執行轉換，確保所有 illegal ops 都被轉換

---

### 處理 Dialect Subset 不匹配的情況

#### 場景：不同 NPU 支援不同的操作集合

```text
High-level (Linalg):
{conv_2d, matmul, add, relu, pooling}
           ↓
     [LOWERING]
           ↓
Dialect A:              Dialect B:
{P, Q, R, S}           {K, M, N, O, P}
  ↑                      ↑
  P (common)            P (common)
  Q (A-specific)        K (B-specific)
  R (A-specific)        M (B-specific)
  S (A-specific)        N (B-specific)
                        O (B-specific)
```

**Challenge:**

- 如何 lower `linalg.conv_2d`？
  - 在 Dialect A 上：使用 operation Q
  - 在 Dialect B 上：使用 operation K
- 兩個 dialect 有不同的操作集合

#### 解決方案 1: 條件式 Lowering (Target-Aware Patterns)

```cpp
// linalg.conv_2d 的 lowering 根據 target 不同
class ConvertLinalgConv2D : public OpConversionPattern<linalg::Conv2DNhwcHwcfOp> {
public:
  ConvertLinalgConv2D(MLIRContext *ctx, StringRef targetName)
      : OpConversionPattern(ctx), target(targetName) {}

  LogicalResult matchAndRewrite(linalg::Conv2DNhwcHwcfOp op,
                                 OpAdaptor adaptor,
                                 ConversionPatternRewriter &rewriter) const override {
    Location loc = op.getLoc();

    if (target == "npu_a") {
      // NPU A: 使用 operation Q (假設 Q 是 im2col-based conv)
      auto im2col = rewriter.create<npu_a::Im2ColOp>(loc, adaptor.getInput());
      auto gemm = rewriter.create<npu_a::GemmOp>(loc, im2col, adaptor.getFilter());
      rewriter.replaceOpWithNewOp<npu_a::Col2ImOp>(op, gemm);

    } else if (target == "npu_b") {
      // NPU B: 使用 operation K (假設 K 是 direct conv)
      rewriter.replaceOpWithNewOp<npu_b::DirectConvOp>(
          op, adaptor.getInput(), adaptor.getFilter());

    } else {
      return failure();
    }

    return success();
  }

private:
  StringRef target;
};

// 使用時指定 target
void populateLinalgToNPU_A_Patterns(RewritePatternSet &patterns) {
  patterns.add<ConvertLinalgConv2D>(patterns.getContext(), "npu_a");
}

void populateLinalgToNPU_B_Patterns(RewritePatternSet &patterns) {
  patterns.add<ConvertLinalgConv2D>(patterns.getContext(), "npu_b");
}
```

#### 解決方案 2: Separate Pattern Files (推薦)

```text
lib/Dialect/
├── NPU_A/
│   └── Transforms/
│       └── LinalgToNPU_A.cpp      # 只包含 NPU A 的 patterns
│           ├── linalg.conv_2d → npu_a.op_q
│           ├── linalg.matmul  → npu_a.op_p
│           └── linalg.pooling → npu_a.op_r
│
└── NPU_B/
    └── Transforms/
        └── LinalgToNPU_B.cpp      # 只包含 NPU B 的 patterns
            ├── linalg.conv_2d → npu_b.op_k
            ├── linalg.matmul  → npu_b.op_p
            └── linalg.pooling → npu_b.op_m
```

```cpp
// File: lib/Dialect/NPU_A/Transforms/LinalgToNPU_A.cpp
class ConvertConv2DToNPU_A : public OpRewritePattern<linalg::Conv2DNhwcHwcfOp> {
  // ... NPU A specific implementation using op Q
};

void populateLinalgToNPU_A_Patterns(RewritePatternSet &patterns) {
  patterns.add<ConvertConv2DToNPU_A>(patterns.getContext());
  patterns.add<ConvertMatMulToNPU_A>(patterns.getContext());
  patterns.add<ConvertPoolingToNPU_A>(patterns.getContext());
}

// File: lib/Dialect/NPU_B/Transforms/LinalgToNPU_B.cpp
class ConvertConv2DToNPU_B : public OpRewritePattern<linalg::Conv2DNhwcHwcfOp> {
  // ... NPU B specific implementation using op K
};

void populateLinalgToNPU_B_Patterns(RewritePatternSet &patterns) {
  patterns.add<ConvertConv2DToNPU_B>(patterns.getContext());
  patterns.add<ConvertMatMulToNPU_B>(patterns.getContext());
  patterns.add<ConvertPoolingToNPU_B>(patterns.getContext());
}
```

**優點：**

- ✅ 完全分離的程式碼 (no shared code, no conditionals)
- ✅ 容易維護和測試
- ✅ 可以獨立開發不同 target 的 backend

#### 解決方案 3: Common Abstraction Layer (Hybrid)

```text
High-level (Linalg)
      ↓
[Common lowering]
      ↓
NPU Common Dialect (抽象層)
{generic_conv, generic_matmul, generic_pool}
      ↓
[Target-specific lowering]
      ↓
  ┌───┴───┐
  ↓       ↓
NPU_A    NPU_B
{P,Q,R,S} {K,M,N,O,P}
```

```cpp
// Step 1: Linalg → NPU Common (shared)
class ConvertLinalgToNPUCommon : public OpRewritePattern<linalg::Conv2DNhwcHwcfOp> {
  LogicalResult matchAndRewrite(...) {
    rewriter.replaceOpWithNewOp<npu_common::GenericConvOp>(op, ...);
    return success();
  }
};

// Step 2: NPU Common → NPU A (target-specific)
class ConvertNPUCommonToNPU_A : public OpRewritePattern<npu_common::GenericConvOp> {
  LogicalResult matchAndRewrite(...) {
    // Use NPU A's specific operation Q
    rewriter.replaceOpWithNewOp<npu_a::OpQ>(op, ...);
    return success();
  }
};

// Step 3: NPU Common → NPU B (target-specific)
class ConvertNPUCommonToNPU_B : public OpRewritePattern<npu_common::GenericConvOp> {
  LogicalResult matchAndRewrite(...) {
    // Use NPU B's specific operation K
    rewriter.replaceOpWithNewOp<npu_b::OpK>(op, ...);
    return success();
  }
};
```

**優點：**

- ✅ 共用的 high-level → common 轉換
- ✅ Target-specific 的 common → target 轉換
- ⚠️ 需要維護額外的 common dialect (增加複雜度)

---

### Summary: Lowering 實作的關鍵要點

#### ✅ Key Takeaways

1. **Lowering ≠ 1-1 Mapping**

   - 可以是 1→N (decomposition)
   - 可以是 N→1 (fusion)
   - 可以是 N→M (reorganization)

2. **MLIR 不需要 Cost Model**

   - 不像 LLVM iSel 需要計算最小權重
   - 使用 declarative pattern rewriting
   - 每個 pattern 定義明確的語意轉換

3. **Pattern Rewriting 三要素**

   - `OpRewritePattern`: 定義轉換規則
   - `ConversionTarget`: 定義 legal/illegal ops
   - `applyPartialConversion`: 執行轉換

4. **多 Target 支援策略**
   - **推薦**: Separate pattern files per target
   - **可選**: Conditional patterns with target parameter
   - **進階**: Common abstraction layer (hybrid approach)

#### 📊 Lowering Patterns 設計決策表

| Question                 | Answer                 | Reason                                                       |
| ------------------------ | ---------------------- | ------------------------------------------------------------ |
| 需要 cost model 嗎？     | ❌ No                  | MLIR 使用確定性的語意轉換                                    |
| 可以 1→N 嗎？            | ✅ Yes                 | Decomposition 很常見                                         |
| 可以 N→1 嗎？            | ✅ Yes                 | Fusion optimization                                          |
| 如何支援多 target？      | Separate pattern files | 最清晰、最容易維護                                           |
| Pattern 執行順序重要嗎？ | ⚠️ Sometimes           | 某些 patterns 需要先執行 (e.g., fusion before decomposition) |

---

## Questions

### Convoluiton lowering

#### Q1: How convolution lowering works in MLIR?

Convolution lowering in MLIR follows a multi-stage transformation process:

**Stage 1: Framework → Linalg (PROVIDED)**

```text
torch.aten.conv2d → linalg.conv_2d_nhwc_hwcf
tf.nn.conv2d → linalg.conv_2d_nhwc_hwcf
```

**Stage 2: Linalg → Target Dialect (YOU IMPLEMENT)**

The lowering strategy depends on your target hardware capabilities:

**Option A: Direct Mapping (1→1)**

```cpp
// Simple case: NPU has native conv2d instruction
class ConvertLinalgConv2DToNPU : public OpRewritePattern<linalg::Conv2DNhwcHwcfOp> {
  LogicalResult matchAndRewrite(linalg::Conv2DNhwcHwcfOp op,
                                PatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<npu::Conv2DOp>(
        op, op.getInput(), op.getFilter(),
        op.getStrides(), op.getPadding());
    return success();
  }
};
```

**Option B: Decomposition (1→N)**

```cpp
// Complex case: NPU requires explicit memory management
class ConvertLinalgConv2DToNPU : public OpRewritePattern<linalg::Conv2DNhwcHwcfOp> {
  LogicalResult matchAndRewrite(linalg::Conv2DNhwcHwcfOp op,
                                PatternRewriter &rewriter) const override {
    Location loc = op.getLoc();

    // Step 1: DMA load input to local memory
    auto inputLocal = rewriter.create<npu::DMALoadOp>(
        loc, op.getInput(), /*local_mem=*/true);

    // Step 2: DMA load filter to local memory
    auto filterLocal = rewriter.create<npu::DMALoadOp>(
        loc, op.getFilter(), /*local_mem=*/true);

    // Step 3: Perform convolution
    auto convResult = rewriter.create<npu::Conv2DOp>(
        loc, inputLocal, filterLocal, op.getStrides(), op.getPadding());

    // Step 4: DMA store result back to global memory
    rewriter.replaceOpWithNewOp<npu::DMAStoreOp>(
        op, convResult, /*global_mem=*/true);

    return success();
  }
};
```

**Option C: Algorithm-Specific Lowering**

```cpp
// Advanced case: Lower to im2col + GEMM for better optimization
class ConvertConv2DToGEMM : public OpRewritePattern<linalg::Conv2DNhwcHwcfOp> {
  LogicalResult matchAndRewrite(linalg::Conv2DNhwcHwcfOp op,
                                PatternRewriter &rewriter) const override {
    // Conv2D = im2col(input) × reshape(filter) + col2im(result)
    auto im2colResult = rewriter.create<npu::Im2ColOp>(loc, op.getInput(), ...);
    auto filterReshaped = rewriter.create<npu::ReshapeOp>(loc, op.getFilter(), ...);
    auto gemmResult = rewriter.create<npu::GEMMOp>(loc, im2colResult, filterReshaped);
    rewriter.replaceOpWithNewOp<npu::Col2ImOp>(op, gemmResult, ...);
    return success();
  }
};
```

**Stage 3: Optimization (OPTIONAL)**

```cpp
// Fuse conv + activation for better performance
class FuseConvRelu : public OpRewritePattern<npu::ReLUOp> {
  LogicalResult matchAndRewrite(npu::ReLUOp relu, PatternRewriter &rewriter) const override {
    auto conv = relu.getInput().getDefiningOp<npu::Conv2DOp>();
    if (!conv || !conv.getResult().hasOneUse()) return failure();

    // Replace conv + relu with fused operation
    rewriter.replaceOpWithNewOp<npu::FusedConvReluOp>(
        relu, conv.getInput(), conv.getFilter(), conv.getStrides(), conv.getPadding());
    rewriter.eraseOp(conv);
    return success();
  }
};
```

#### Q2: Does NPU dialect usually have a convolution operator?

**Yes, most NPU dialects include convolution operators**, but the design varies by hardware capabilities:

**Typical NPU Dialect Operations:**

| Operation Category    | Common NPU Ops                                    | Purpose                         |
| --------------------- | ------------------------------------------------- | ------------------------------- |
| **Convolution**       | `npu.conv2d`, `npu.depthwise_conv`, `npu.conv3d`  | Core neural network operations  |
| **Matrix Ops**        | `npu.matmul`, `npu.gemm`, `npu.batch_matmul`      | Dense layer computations        |
| **Activation**        | `npu.relu`, `npu.sigmoid`, `npu.gelu`             | Non-linear functions            |
| **Pooling**           | `npu.max_pool`, `npu.avg_pool`, `npu.global_pool` | Spatial reduction               |
| **Memory**            | `npu.dma_load`, `npu.dma_store`, `npu.copy`       | Data movement                   |
| **Fused**             | `npu.conv_relu`, `npu.conv_bn`, `npu.matmul_add`  | Hardware-optimized combinations |
| **Hardware-Specific** | `npu.winograd_conv`, `npu.quantized_conv`         | Vendor optimizations            |

**Design Considerations:**

1. **Granularity**: Balance between high-level ops (easy to optimize) vs low-level ops (close to hardware)

2. **Memory Hierarchy**: Explicit memory management for NPUs with complex memory systems

3. **Fusion Opportunities**: Hardware-specific fused operations for better performance

4. **Data Types**: Support for quantized types (int8, int16) common in NPU inference

**Example NPU Dialect Definition:**

```cpp
// npu.conv2d operation
def NPU_Conv2DOp : NPU_Op<"conv2d"> {
  let summary = "2D convolution operation for NPU";
  let arguments = (ins
    AnyTensor:$input,
    AnyTensor:$filter,
    OptionalAttr<I64ArrayAttr>:$strides,
    OptionalAttr<I64ArrayAttr>:$padding,
    OptionalAttr<StrAttr>:$data_format,
    OptionalAttr<BoolAttr>:$use_local_memory
  );
  let results = (outs AnyTensor:$output);

  let assemblyFormat = [{
    $input `,` $filter
    (`strides` `=` $strides^)?
    (`padding` `=` $padding^)?
    (`local_mem` $use_local_memory^)?
    attr-dict `:` functional-type(operands, results)
  }];
}
```

#### Q3: What are the Common Tensor Operations in NPU Dialects?

NPU dialects typically support a comprehensive set of tensor operations that cover the full spectrum of neural network computations. These operations can be categorized into several groups:

**1. Core Computational Layers (計算密集型操作)**

These are the most compute-intensive operations and the primary targets for NPU hardware acceleration, typically accounting for 80%+ of model execution time.

| Operation                 | MLIR Op                                               | Purpose                                      | Applications                                            |
| ------------------------- | ----------------------------------------------------- | -------------------------------------------- | ------------------------------------------------------- |
| **Convolution**           | `npu.conv2d`, `npu.conv3d`, `npu.depthwise_conv`      | Sliding kernel feature extraction            | CNN backbone: image classification, object detection    |
| **Matrix Multiplication** | `npu.matmul`, `npu.gemm`, `npu.batch_matmul`          | Dense linear transformations                 | Fully connected layers, RNN/LSTM, Transformer attention |
| **Pooling**               | `npu.max_pool`, `npu.avg_pool`, `npu.global_avg_pool` | Spatial downsampling and feature aggregation | Feature map reduction, spatial invariance               |

**2. Activation Functions (激活函數)**

Element-wise non-linear functions that introduce non-linearity to enable learning complex patterns.

| Operation        | MLIR Op                                               | Mathematical Definition             | Primary Use Cases                            |
| ---------------- | ----------------------------------------------------- | ----------------------------------- | -------------------------------------------- |
| **ReLU Family**  | `npu.relu`, `npu.leaky_relu`, `npu.prelu`, `npu.gelu` | `f(x) = max(0, x)` and variants     | Most widely used activation functions        |
| **Sigmoid/Tanh** | `npu.sigmoid`, `npu.tanh`                             | `σ(x) = 1/(1+e^(-x))`, `tanh(x)`    | RNN gating mechanisms, binary classification |
| **Softmax**      | `npu.softmax`                                         | `softmax(x_i) = e^(x_i) / Σe^(x_j)` | Multi-class classification output layer      |

**3. Data Shape & Structuring Operations (資料維度操作)**

These operations don't involve complex mathematical computations but are critical for data flow and tensor manipulation.

| Operation             | MLIR Op                        | Purpose                                   | Example                                |
| --------------------- | ------------------------------ | ----------------------------------------- | -------------------------------------- |
| **Reshape/Flatten**   | `npu.reshape`, `npu.flatten`   | Change tensor shape without altering data | `1x28x28x1` → `784x1` for FC layer     |
| **Concatenation**     | `npu.concat`                   | Join tensors along specified dimension    | Feature fusion from multiple branches  |
| **Transpose/Permute** | `npu.transpose`, `npu.permute` | Reorder tensor dimensions                 | `NCHW` ↔ `NHWC` layout conversion      |
| **Slice/Pad**         | `npu.slice`, `npu.pad`         | Extract subregions or add padding         | Crop operations, zero-padding for conv |

**4. Normalization Layers (正規化層)**

Critical for training stability and model convergence.

| Operation                  | MLIR Op             | Purpose                            | Application                     |
| -------------------------- | ------------------- | ---------------------------------- | ------------------------------- |
| **Batch Normalization**    | `npu.batch_norm`    | Normalize across batch dimension   | CNN layers (training/inference) |
| **Layer Normalization**    | `npu.layer_norm`    | Normalize across feature dimension | Transformer models              |
| **Instance Normalization** | `npu.instance_norm` | Normalize per instance             | Style transfer, GANs            |

**5. Basic Element-wise Arithmetic (基礎逐元素運算)**

Fundamental tensor operations for combining and transforming data.

| Operation            | MLIR Op                                               | Mathematical Definition            | Key Applications                       |
| -------------------- | ----------------------------------------------------- | ---------------------------------- | -------------------------------------- |
| **Basic Arithmetic** | `npu.add`, `npu.sub`, `npu.mul`, `npu.div`            | Element-wise: `C[i] = A[i] ⊕ B[i]` | ResNet skip connections, bias addition |
| **Comparison**       | `npu.greater`, `npu.less`, `npu.equal`                | Element-wise comparisons           | Conditional operations, masking        |
| **Reduction**        | `npu.reduce_sum`, `npu.reduce_mean`, `npu.reduce_max` | Reduce along specified dimensions  | Global pooling, loss computation       |

**6. Advanced/Specialized Operations (進階操作)**

Hardware-specific or algorithm-optimized operations.

| Operation                | MLIR Op                                               | Purpose                               | Benefits                                     |
| ------------------------ | ----------------------------------------------------- | ------------------------------------- | -------------------------------------------- |
| **Fused Operations**     | `npu.conv_relu`, `npu.matmul_add`, `npu.conv_bn_relu` | Combine multiple ops in single kernel | Reduced memory bandwidth, better performance |
| **Quantized Operations** | `npu.qconv2d`, `npu.qmatmul`                          | Low-precision arithmetic (int8/int16) | Faster inference, reduced memory usage       |
| **Winograd Convolution** | `npu.winograd_conv`                                   | Algorithm-optimized convolution       | Reduced arithmetic complexity for 3x3 conv   |

**NPU Dialect Design Principles:**

1. **Performance-Oriented**: Operations designed to maximize hardware utilization
2. **Memory-Aware**: Explicit control over data movement and memory hierarchy
3. **Fusion-Friendly**: Support for operation fusion to reduce memory traffic
4. **Quantization-Ready**: Native support for reduced precision arithmetic
5. **Layout-Flexible**: Support for different tensor layouts (NCHW, NHWC, etc.)

**Key Insight**: The convolution operator in NPU dialects is typically **more specific** than the generic `linalg.conv_2d`, including hardware-specific attributes like memory placement, data layout preferences, and optimization hints.

### Compiler Design Insights (編譯器設計洞察)

**Performance Optimization Principles:**

1. **80/20 Rule - Focus on Core Computational Layers**

   - The **Core Computational Layers** (Category 1) are the performance bottleneck
   - These are the **highest priority** for hardware-specific NPU dialect operations
   - Design principle: Optimize the operations that consume 80% of execution time

2. **Fusion Opportunities - Eliminate Memory Traffic**

   - **Activation Functions** (Category 2) and **Basic Arithmetic** (Category 5) rarely exist independently
   - **Compiler's key responsibility**: Use **Operator Fusion** to merge them into Core Computational Layers
   - **Benefit**: Eliminate intermediate memory reads/writes

   ```cpp
   // Before fusion: 2 separate operations + memory traffic
   %conv = npu.conv2d %input, %filter
   %relu = npu.relu %conv

   // After fusion: 1 operation, no intermediate memory
   %result = npu.conv_relu %input, %filter
   ```

3. **Performance Killers - Data Layout Operations**

   - **Data Shape & Structuring Operations** (Category 3) are performance bottlenecks
   - A single unnecessary `Transpose` can completely negate NPU acceleration gains
   - **Root cause**: Massive memory movement overhead
   - **Solution**: **Layout-Aware Optimization** to minimize or eliminate these operations

   ```cpp
   // Performance killer example:
   %nhwc = npu.transpose %nchw  // Expensive memory reshuffling
   %conv = npu.conv2d %nhwc, %filter

   // Optimized approach:
   %conv = npu.conv2d %nchw, %filter {layout="nchw"}  // Native layout support
   ```

**Compiler Design Strategy:**

| Priority         | Operation Category    | Optimization Strategy          | Implementation            |
| ---------------- | --------------------- | ------------------------------ | ------------------------- |
| **🔴 Critical**  | Core Computational    | Hardware-specific ops + Fusion | Custom NPU instructions   |
| **🟡 Important** | Activation/Arithmetic | Fuse with core ops             | Pattern rewriting         |
| **⚠️ Minimize**  | Data Layout           | Layout-aware optimization      | Avoid/optimize transposes |

## Layout-Aware Optimization

**Definition:** Layout-Aware Optimization is a compiler optimization strategy where the compiler deeply understands the physical memory layout of data and proactively selects and transforms data layouts based on target hardware characteristics to generate the highest-performance execution code.

**Core Principle:** Instead of passively accepting input data layouts, the compiler actively plans the optimal layout path for the entire computation process.

### Why NPU Requires Layout-Aware Optimization

NPUs are extremely sensitive to data layout optimization - it's one of the **most critical and differentiating optimization tasks** in NPU compiler design.

#### Hardware Architecture Constraints

| Factor                         | Impact                                                                                         | Example                                                            |
| ------------------------------ | ---------------------------------------------------------------------------------------------- | ------------------------------------------------------------------ |
| **Hardware Design Preference** | NPU compute units (e.g., Systolic Arrays) are physically designed for specific data flows      | NPU optimized for 16-channel parallel reads: NHWC >> NCHW          |
| **Memory Access Efficiency**   | Hardware achieves maximum efficiency through coalesced (contiguous) memory access              | 2x2 patch processing: contiguous layout = 1 read vs 4 reads        |
| **SRAM Utilization**           | Limited on-chip SRAM determines how data tiling affects compute unit utilization ("feed rate") | Optimal tiling layout keeps compute units busy vs waiting for data |

**Performance Impact:** Wrong layout can reduce NPU hardware utilization to **<10%** of peak performance.

#### Layout Optimization Trade-offs

**Warehouse Management Analogy:**

```text
❌ Layout-Unaware (Passive):
Goods (data) → Store as received → Robotic arm (compute unit) travels long distances
Result: Low efficiency, single-item retrieval

✅ Layout-Aware (Active):
Goods (data) → Smart reorganization → Robotic arm (compute unit) optimal access pattern
Result: High efficiency, batch processing
```

### Implementation Strategy

Layout-Aware Optimization is implemented as a **global, cost-model-based compiler pass** with the following phases:

#### Phase 1: Hardware Model Construction

**Purpose:** Define NPU layout preferences and transformation costs.

```cpp
// Hardware model definition using MLIR TableGen
def NPU_HardwareModel : HardwareModel {
  let layoutPreferences = [
    OpLayoutPreference<"npu.conv2d", [NHWC_Layout], 100>,      // High preference
    OpLayoutPreference<"npu.matmul", [NCHW_Layout], 80>,       // Medium preference
  ];

  let transformationCosts = [
    LayoutTransformCost<NCHW_to_NHWC, 50>,    // Transpose cost
    LayoutTransformCost<NHWC_to_NCHW, 50>,    // Transpose cost
  ];
}
```

#### Phase 2: Layout Constraint Propagation

**Process:** Traverse computation graph and propagate layout constraints from fixed points.

```text
Input Image (NHWC) → Conv2D → ReLU → MatMul (prefers NCHW) → Output
     ↓                ↓       ↓          ↓
  Fixed layout    Inherits   Inherits   Conflict detected!
```

#### Phase 3: Cost Analysis & Decision Making

**Decision Point:** When operations have conflicting layout preferences.

| Scenario              | Option A                           | Option B                               | Decision Criteria         |
| --------------------- | ---------------------------------- | -------------------------------------- | ------------------------- |
| **Op A** outputs NCHW | Insert `transpose` before **Op B** | Force **Op A** to output NHWC directly | **Total cost** comparison |
| **Op B** prefers NHWC | Cost = transpose_cost              | Cost = suboptimal_Op_A_cost            | Choose minimum total cost |

**Cost Model Example:**

```cpp
// Cost calculation for layout decision
float calculateTotalCost(LayoutPath path) {
  float computeCost = 0;
  float transformCost = 0;

  for (auto op : path.operations) {
    computeCost += getOpCost(op, path.getLayout(op));
  }

  for (auto transform : path.transformations) {
    transformCost += getTransformCost(transform);
  }

  return computeCost + transformCost;
}
```

#### Phase 4: Transformation Node Insertion

**Result:** Explicit layout conversion operations in the final IR.

```cpp
// Before optimization: Implicit layout mismatch
%conv_result = npu.conv2d %input, %filter  // Outputs NCHW
%matmul_result = npu.matmul %conv_result, %weights  // Expects NHWC

// After optimization: Explicit layout conversion
%conv_result = npu.conv2d %input, %filter  // Outputs NCHW
%transposed = npu.transpose %conv_result {perm = [0, 2, 3, 1]}  // NCHW → NHWC
%matmul_result = npu.matmul %transposed, %weights  // Uses NHWC
```

### Layout Optimization Examples

#### Example 1: Eliminating Redundant Transposes

```cpp
// Suboptimal: Multiple layout conversions
%nhwc_1 = npu.transpose %nchw {perm = [0, 2, 3, 1]}  // NCHW → NHWC
%conv = npu.conv2d %nhwc_1, %filter                  // Prefers NHWC
%nchw_2 = npu.transpose %conv {perm = [0, 3, 1, 2]}  // NHWC → NCHW
%pool = npu.max_pool %nchw_2                         // Prefers NCHW

// Optimized: Layout-consistent path
%conv = npu.conv2d %nchw, %filter {layout = "nchw"}  // Direct NCHW support
%pool = npu.max_pool %conv                           // No transpose needed
```

#### Example 2: Global Layout Planning

**Strategy Comparison:**

| Approach       | Layout Decisions             | Transpose Count | Performance   |
| -------------- | ---------------------------- | --------------- | ------------- |
| **Local Opt**  | Greedy per-operation         | 3-4 transposes  | ⚠️ Suboptimal |
| **Global Opt** | End-to-end cost minimization | 0-1 transpose   | ✅ Optimal    |

```cpp
// Global optimization result
%input_hwc = npu.transpose %input_chw {perm = [1, 2, 0]}    // One upfront transpose
%conv1 = npu.conv2d %input_hwc, %f1 {layout = "nhwc"}      // Chain of NHWC ops
%relu1 = npu.relu %conv1
%conv2 = npu.conv2d %relu1, %f2 {layout = "nhwc"}
%relu2 = npu.relu %conv2
%output = npu.global_avg_pool %relu2 {layout = "nhwc"}     // No additional transposes
```

**Performance Insight:** One strategic transpose upfront eliminates 4-5 transposes later in the pipeline.

## Good reference

- [MLIR Rationale](https://mlir.llvm.org/docs/Rationale/)
- [MLIR Glossary](https://mlir.llvm.org/getting_started/Glossary/)
