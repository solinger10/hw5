# PS5 makefile
#
# targets are:
#
# all -- rebuild the project (default)
# ws -- rebuild worker_server
# con -- rebuild controller
# clean -- remove all objects and executables

UTIL_SOURCES = shared/plane.mli shared/plane.ml shared/util.mli shared/util.ml \
shared/simulations.mli shared/simulations.ml

SHARED_SOURCES = shared/thread_pool.mli shared/thread_pool.ml \
shared/connection.mli shared/connection.ml shared/protocol.mli shared/protocol.ml

WS_SOURCES = worker_server/program.mli worker_server/program.ml worker_server/worker.mli worker_server/worker.ml \
worker_server/worker_server.mli worker_server/worker_server.ml

CONTROLLER_SOURCES = controller/worker_manager.mli controller/worker_manager.ml \
controller/map_reduce.mli controller/map_reduce.ml

APP_SOURCES = apps/nbody/nbody.ml apps/word_count/word_count.ml \
apps/grades/grades.ml apps/transaction_track/transaction_track.ml

LIBS = shared worker_server controller apps/nbody apps/word_count \
apps/grades apps/transaction_track

.PHONY: all
all: ws con foo

.PHONY: ws
ws: worker_server.exe

.PHONY: con
con: controller.exe

.PHONY: clean
clean:
	for X in . $(LIBS); do \
      for Y in cmo cmi cma exe; do \
        rm -f $$X/*.$$Y; \
      done; \
    done

shared/util.cma: $(UTIL_SOURCES)
	ocamlc -o shared/util.cma -a -I shared $(UTIL_SOURCES)

foo: shared/util.cma $(SHARED_SOURCES)
	ocamlc -c -thread -I shared -I controller $(SHARED_SOURCES) $(UTIL_SOURCES) $(APP_SOURCES)

worker_server.exe: shared/util.cma $(SHARED_SOURCES) $(WS_SOURCES)
	ocamlc -thread -o worker_server.exe -I shared -I worker_server dynlink.cma str.cma unix.cma \
    threads.cma util.cma $(SHARED_SOURCES) $(WS_SOURCES)

controller.exe: shared/util.cma $(SHARED_SOURCES) $(CONTROLLER_SOURCES) controller/main.ml
	ocamlc -thread -o controller.exe -I shared -I controller -I apps/nbody \
	-I apps/word_count -I apps/grades -I apps/transaction_track dynlink.cma unix.cma \
    threads.cma str.cma util.cma $(SHARED_SOURCES) $(CONTROLLER_SOURCES) controller/main.ml
