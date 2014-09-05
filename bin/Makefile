CSC := dmcs
CSCOPTS :=  /optimize+ /debug- 
CSCSOURCESPFX=../src/cs/dll/

CSCSOURCES0 = Runtime.cs Scripting.cs Parser.cs Core.cs Pure.cs AssemblyInfo.cs version.cs

CSCSOURCES = $(addprefix $(CSCSOURCESPFX), $(CSCSOURCES0))

slash:=/
backslash:=/
CSCWINSOURCES = $(subst $(slash),$(backslash),$(CSCSOURCES))
COMMANDLINEWIN = $(subst $(slash),$(backslash),../src/cs/cl/CommandLine.cs)

LIB = ../src/l/

MONO := mono

MBASE = $(MONO) ./mb0.exe

all: repl.exe mbweave.exe pfront.exe pfrepl.exe pfprint.exe pftexprint.exe \
	mlrepl.exe mlcomp.exe pfhighlight.exe prologrepl.exe MBaseLogic.dll \
	doc

clean:
	rm -f *.dll *.alc *.exe

../doc/pfdoc.pdf: pftexprint.exe ../doc/pfdoc.tex ../src/l/lib/pfront/doc.hll
	(cd ../doc; $(MONO) ../bin/pftexprint.exe pfrontdoc ../src/l/lib/pfront/doc.hll)
	(cd ../doc; pdflatex pfdoc; pdflatex pfdoc)

../doc/doc.pdf: repl.exe ../doc/doc.tex 
	(cd ../doc; pdflatex doc; pdflatex doc)

doc: ../doc/pfdoc.pdf ../doc/doc.pdf

MBaseCore.dll: $(CSCSOURCES)
	$(CSC) $(CSCOPTS) /target:library /out:MBaseCore.dll $(CSCWINSOURCES)

mb0.exe: MBaseCore.dll ../src/cs/cl/CommandLine.cs
	$(CSC) $(CSCOPTS) /out:mb0.exe /r:MBaseCore.dll $(COMMANDLINEWIN)
	./mbfixbin mb0.exe

boot.alc: mb0.exe $(LIB)/boot/boot-bck.alc $(LIB)/boot/boot.al $(LIB)/version.al
	cp $(LIB)/boot/boot-bck.alc ./boot.alc
	$(MBASE) /I boot.alc /BOOT $(LIB)/boot/boot.al

lib.alc: mb0.exe boot.alc $(LIB)/boot/initlib.al $(LIB)/version.al
	$(MBASE) /I boot.alc /C $(LIB)/boot/initlib.al lib.alc

lib1.alc: boot.alc lib.alc $(LIB)/boot/dotnetlib.al $(LIB)/util/makeasmlib.al
	$(MBASE) /I boot.alc /I lib.alc /C $(LIB)/boot/dotnetlib.al lib1.alc
	$(MBASE) /I boot.alc /I lib.alc /I lib1.alc $(LIB)/util/makeasmlib.al

libc.alc: boot.alc lib.alc lib1.alc $(LIB)/boot/common.al $(LIB)/version.al
	$(MBASE) /I boot.alc /I lib.alc /I lib1.alc /C $(LIB)/boot/common.al libc.alc

boot2.alc: boot.alc lib.alc lib1.alc libc.alc
	cat boot.alc lib.alc lib1.alc libc.alc > boot2.alc

libnet.alc: boot2.alc $(LIB)/boot/stagec.al $(LIB)/version.al
	$(MBASE) /I boot2.alc /C $(LIB)/boot/stagec.al libnet.alc

boot2c.dll: boot2.alc libnet.alc $(LIB)/boot/stage2.al $(LIB)/version.al
	$(MBASE) /I boot2.alc /I libnet.alc $(LIB)/boot/stage2.al

boot3.dll: boot2c.dll libnet.alc $(LIB)/boot/stage3.al $(LIB)/version.al
	$(MBASE) /D boot2c.dll /I libnet.alc $(LIB)/boot/stage3.al

boot4.dll: boot3.dll $(LIB)/boot/stage4.al $(LIB)/version.al
	$(MBASE) /D boot3.dll $(LIB)/boot/stage4.al

MBaseBin.dll: boot4.dll $(LIB)/boot/stage5.al $(LIB)/version.al
	$(MBASE) /D boot4.dll $(LIB)/boot/stage5.al

repl.exe: MBaseBin.dll $(LIB)/util/repl.al $(LIB)/version.al
	$(MBASE) /D MBaseBin.dll $(LIB)/util/repl.al
	./mbfixbin ./repl.exe

mbweave.exe: repl.exe $(LIB)/util/weave.al
	$(MONO) ./repl.exe /b /emit $(LIB)/util/weave.al

temporary-source.al: repl.exe $(LIB)/lib/parsing/pegtemp.al
	rm -f $(LIB)/lib/parsing/temporary-source.al
	$(MONO) ./repl.exe /b $(LIB)/lib/parsing/pegtemp.al
	cp temporary-source.al $(LIB)/lib/parsing/

MBasePackrat.dll: repl.exe temporary-source.al $(LIB)/lib/parsing/packrat.al
	$(MONO) ./repl.exe /b /emit $(LIB)/lib/parsing/packrat.al

MBaseFront.dll: MBasePackrat.dll
	$(MONO) ./repl.exe /b /emit $(LIB)/lib/pfront/front.al

MBaseLogic.dll: pfront.exe
	$(MONO) ./pfront.exe $(LIB)/lib/wam/mbaselogic.hl

pfront.exe: MBaseFront.dll $(LIB)/lib/pfront/pfront.al
	$(MONO) ./repl.exe /b /emit $(LIB)/lib/pfront/pfront.al

pfprint.exe: MBaseFront.dll $(LIB)/lib/pfront/pfprint.al
	$(MONO) ./repl.exe /b /emit $(LIB)/lib/pfront/pfprint.al

pftexprint.exe: MBaseFront.dll $(LIB)/lib/pfront/pftexprint.al
	$(MONO) ./repl.exe /b /emit $(LIB)/lib/pfront/pftexprint.al

pfrepl.exe: MBaseFront.dll $(LIB)/lib/pfront/pfrepl.al
	$(MONO) ./repl.exe /b /emit $(LIB)/lib/pfront/pfrepl.al

MBaseML.dll: MBasePackrat.dll
	$(MONO) ./repl.exe /b /emit $(LIB)/lib/ml/mldll.al

mlrepl.exe: MBaseML.dll
	$(MONO) ./repl.exe /b /emit $(LIB)/lib/ml/mlrepl.al

mlcomp.exe: MBaseML.dll
	$(MONO) ./repl.exe /b /emit $(LIB)/lib/ml/mlcomp.al

prologrepl.exe: MBaseLogic.dll
	$(MONO) ./pfront.exe /c prologrepl $(LIB)/lib/wam/prolog_repl.hl

pfhighlight.exe: repl.exe MBasePackrat.dll MBaseFront.dll pfront.exe
	$(MONO) ./pfront.exe /dbg /c pfhighlight ../misc/emacs/pfhighlight.hl


