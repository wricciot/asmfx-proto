OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

# The list of object files for asmfx
ASMFX_CMO=lib.cmo isa.cmo machine.cmo
ASMFX_CMX=$(ASMFX_CMO:.cmo=.cmx)
ASMFX_ML=$(ASMFX_CMO:.cmo=.ml)
ASMFX_MLI=$(ASMFX_CMO:.cmo=.mli)

ML=$(ASMFX_ML)
MLI=$(ASMFX_MLI)

asmfx: $(ASMFX_CMO)
	$(OCAMLC) -o asmfx $(OCAMLFLAGS) $(ASMFX_CMO)


asmfx.opt: $(ASMFX_CMX)
	$(OCAMLOPT) -o asmfx $(OCAMLFLAGS) $(ASMFX_CMX)

# Common rules

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

all: asmfx

opt: asmfx.opt

# Clean up
clean:
	rm -f asmfx asmfx.opt
	rm -f *.cm[iox]

# Dependencies
.depend: $(ML) $(MLI)
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

.PHONY: all opt clean 

include .depend
