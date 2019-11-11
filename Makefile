raw_cpy = $(wildcard raw/COPYBOOK.*)
cpy_files = $(patsubst raw/COPYBOOK.%,include/%.cpy,$(raw_cpy))

program_files = ASCOE500 ASCOE515 ASHMA440 ASHMA550 ASHMA660 ASHMA825	\
ASHMA827 ASHMA828 ASHMA830 ASHMA832 ASHMA836 ASHMA840 ASHMA845		\
ASHMA850 ASHMA855 ASHMA857 ASHMA859 ASHMA921 ASHMA922 ASHMA923		\
ASREA001 ASREA007 ASREA018 ASREA110 ASREA151 ASREA352 ASREA743		\
ASREA744 ASREA745 ASREA841 ASREA847 ASREA863 ASREA866 ASREA868		\
ASREA869 ASREA872 ASREA874 ASREA880 ASREA881 ASREA909 ASSRQ560		\
ASSRQ561 CLREB020 CLRTM751 CLRTM752 CLRTM753 CLRTM755 CLRTM756		\
CLRTM757 CLRTM759

subprogram_files = subprogram/ASHMA831.cbl subprogram/ASHMA839.cbl	\
subprogram/ASREA002.cbl subprogram/ASREA003.cbl				\
subprogram/ASREA005.cbl subprogram/ASREA010.cbl				\
subprogram/ASREA105.cbl subprogram/ASREA178.cbl				\
subprogram/ASREA740.cbl subprogram/ASREA741.cbl				\
subprogram/ASREA742.cbl subprogram/ASREA748.cbl				\
subprogram/ASREA852.cbl subprogram/ASREA853.cbl				\
subprogram/ASREA856.cbl subprogram/ASREA859.cbl				\
subprogram/ASREA864.cbl subprogram/CLRTM356.cbl				\
subprogram/CLRTM749.cbl subprogram/CLRTM750.cbl

cbl_files = $(patsubst %,%.cbl,$(program_files)) \
            $(subprogram_files)

ezt_files = easytrieve/ASHMA360.ezt easytrieve/ASHMA361.ezt	\
easytrieve/ASHMA441.ezt easytrieve/ASHMA856.ezt			\
easytrieve/ASREA004.ezt easytrieve/ASREA006.ezt			\
easytrieve/ASREA016.ezt easytrieve/ASREA037.ezt			\
easytrieve/ASREA828.ezt easytrieve/ASREA829.ezt			\
easytrieve/ASREA876.ezt easytrieve/ASREA896.ezt			\
easytrieve/ASREA897.ezt easytrieve/ASREA898.ezt			\
easytrieve/ASREA899.ezt easytrieve/CLRTM748.ezt			\
easytrieve/CLSRQ074.ezt

all : $(cpy_files) $(cbl_files) $(ezt_files) include/EQVAL749RD.cpy  include/EQVAL749FD.cpy $(program_files)

include/%.cpy : raw/COPYBOOK.%
	cat $< | colrm 1 1 | \
            sed 's/++INCLUDE \(.*\)/COPY \1./g' | \
            sed 's///g' > $@

.INTERMEDIATE : include/EQVAL749.FD.cpy
include/EQVAL749FD.cpy : include/EQVAL749.FD.cpy
	mv $< $@

.INTERMEDIATE : include/EQVAL749.RD.cpy
include/EQVAL749RD.cpy : include/EQVAL749.RD.cpy
	mv $< $@

%.cbl : raw/PROGRAM.%
	cat $< | colrm 1 1 | \
            sed 's/++INCLUDE \(.*\)/COPY \1./g' | \
            sed 's/AFTER POSITIONING \([1-3]\)/AFTER ADVANCING \1/g' | \
            sed 's/AFTER POSITIONING 0/AFTER PAGE/g' | \
            sed 's/EXAMINE/INSPECT/g' | \
            sed 's///g' > $@

subprogram/%.cbl : raw/PROGRAM.%
	cat $< | colrm 1 1 | \
            sed 's/++INCLUDE \(.*\)/COPY \1./g' | \
            sed 's/AFTER POSITIONING \([1-3]\)/AFTER ADVANCING \1/g' | \
            sed 's/AFTER POSITIONING 0/AFTER PAGE/g' | \
            sed 's/EXAMINE/INSPECT/g' | \
            sed 's///g' > $@

# http://www.simotime.com/maccpy01.htm
%.ezt : raw/PROGRAM.%
	cat $< | colrm 1 1 | sed 's///g' > $@

% : %.cbl
	cobc -I include -std=ibm-strict -x $^
