structure GPUKernels = 
struct
  open Real
  val mandel_gpu = 
    _import "mandel_gpu" public : MLton.Pointer.t * int * int * int * real *
    real * real * real -> unit;
  
  val test_cuda = 
    _import "test_cuda" public : unit -> unit;
  
  val cublasSGEMM = 
    _import "cublasSGEMM" public : MLton.Pointer.t * MLton.Pointer.t * MLton.Pointer.t * int * int *
     int -> unit;

end
