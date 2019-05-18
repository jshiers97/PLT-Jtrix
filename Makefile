# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh

# "make all" builds the executable as well as the "matrix" library designed
# to test linking external code

.PHONY : all
all : jtrix.native matrix.o 

# "make jtrix.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

jtrix.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind jtrix.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff
	rm matrix.o

# Building the tarball

TESTS = \
  array1 array2 array3 cast1 cast2 dimi dimf expr1 expr2 expr3 for1 if1 \
  if2 if3 matrix1 matrix2 matrix3 opf opi return sizei sizef splicecolf \
  splicecoli splicerowf splicerowi switchrowf switchrowi transposei \
  transposef while1 while2

FAILS = \
  array1 array2 array3 expr1 expr2 expr3 expr4 for1 for2 free matrix1 \
  matrix2 return while1 while2

TESTFILES = $(TESTS:%=test-%.jtrix) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.jtrix) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags jtrix.ml parse.mly \
	README scanner.mll semant.ml testall.sh \
	arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=tests/%) 

jtrix.tar.gz : $(TARFILES)
	cd .. && tar czf PLT-Jtrix/jtrix.tar.gz \
		$(TARFILES:%=PLT-Jtrix/%)
