# $Id: Makefile,v 1.4 2009-02-04 21:54:08 pouzet Exp $
OBJ = stocks.cmo main.cmo
BIN = stocks

OCAMLC = ocamlc
LUCYC = lucyc 

$(BIN): $(OBJ) 
	$(OCAMLC) -o $(BIN) -custom unix.cma $(OBJ) 

main.ml: stocks.ml
	$(LUCYC) -s main -sampling 0 stocks.ls

clean:
	/bin/rm -f *~ *.cmi *.cmo *.cmx *.o *.lci *.dcc stocks.ml main.ml

realclean: clean
	rm -f $(BIN)

.SUFFIXES: .ls .ml .cmo .dcc .mli .cmi .lci

.ls.ml:
	$(LUCYC) $<
.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.mli.lci:
	$(LUCYC) $<

#dependences
