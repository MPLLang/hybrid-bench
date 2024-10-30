structure ForkJoin =
struct
  open ForkJoin

  fun choice {prefer_cpu, prefer_gpu} = prefer_cpu ()
end
