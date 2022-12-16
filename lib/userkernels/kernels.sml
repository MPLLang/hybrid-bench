structure GPUKernels = 
struct
  open Real
  val chooseSGEMM = 
    _import "chooseSGEMM" public : MLton.Pointer.t * MLton.Pointer.t * MLton.Pointer.t * int * int *
     int -> unit;

  val chooseSGEMMInt = 
    _import "chooseSGEMM" public : int array * int array * int array * int * int *
     int -> unit;

  val mandel_gpu = 
    _import "mandel_gpu" public : MLton.Pointer.t * int * int * int * real *
    real * real * real -> unit;
  
  val reduction = 
    _import "reduction" public : int array * int * int -> int;

  val allocCudaManagedMemory =
    _import "allocCudaManagedMemory" public: Int64.int -> MLton.Pointer.t;

  val freeCudaMemory =
    _import "freeCudaMemory" public: MLton.Pointer.t -> unit;

  val reductionManaged = 
    _import "reduction_managed" public : int array * int * int -> int;
  
  val cublasSGEMM = 
    _import "cublasSGEMM" public : MLton.Pointer.t * MLton.Pointer.t * MLton.Pointer.t * int * int *
     int -> unit;

end
