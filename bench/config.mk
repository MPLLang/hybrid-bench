# Various shared configuration useful to most (or all) benchmarks - at
# least the ones that use Futhark.
#
# The variables defined here can be overwritten by environment variables,
# or in the 'make' command itself, e.g.:
#
#   make MPL=/foo/bar/bin/mpl FUTHARK_BACKEND=hip

# Choice of Futhark backend.
FUTHARK_BACKEND ?= cuda

# Path to MPL binary.
MPL ?= /home/ec2-user/proj/mpl/hybrid-sched/build/bin/mpl

MLTONFLAGS = \
  -default-ann 'allowFFI true' \
	-default-ann 'allowLineComments true' \
	-default-type int64 \
	-default-type word64 \
	-disable-pass splitTypes1 \
	-disable-pass splitTypes2

OPENBLAS_MLTONFLAGS = \
	-cc-opt '-I/home/ec2-user/openblas-v0.3.23/installed/include/' \
	-link-opt '-L/home/ec2-user/openblas-v0.3.23/installed/lib/ -lopenblas'

ifeq ($(FUTHARK_BACKEND), c)
MLTONFLAGS += \
	-cc-opt '-I../common/ -D_GNU_SOURCE' \
	-link-opt '-rdynamic'
else ifeq ($(FUTHARK_BACKEND), cuda)
MLTONFLAGS += \
	-cc-opt '-I/opt/nvidia/cuda/include/ -I../common/ -D_GNU_SOURCE' \
	-link-opt '-rdynamic -L/opt/nvidia/cuda/lib64/ -lcuda -lnvrtc -lcublas -lcudart -lstdc++'
else ifeq ($(FUTHARK_BACKEND), hip)
MLTONFLAGS += \
	-cc-opt '-I../common/ -D_GNU_SOURCE' \
	-link-opt '-lamdhip64 -lhiprtc'
else ifeq ($(FUTHARK_BACKEND), opencl)
MLTONFLAGS += \
	-cc-opt '-I../common/ -D_GNU_SOURCE' \
	-link-opt '-lOpenCL'
else
error $(error Unsupported Futhark backend: $(FUTHARK_BACKEND))
endif
