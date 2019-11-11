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

subprogram_files = ASHMA831.cbl ASHMA839.cbl ASREA002.cbl		\
ASREA003.cbl ASREA005.cbl ASREA010.cbl ASREA105.cbl ASREA178.cbl	\
ASREA740.cbl ASREA741.cbl ASREA742.cbl ASREA748.cbl ASREA852.cbl	\
ASREA853.cbl ASREA856.cbl ASREA859.cbl ASREA864.cbl CLRTM356.cbl	\
CLRTM749.cbl CLRTM750.cbl

cbl_files = $(patsubst %,%.cbl,$(program_files)) \
            $(patsubst %,%.cbl,$(subprogram_files))

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

# http://www.simotime.com/maccpy01.htm
%.ezt : raw/PROGRAM.%
	cat $< | colrm 1 1 | sed 's///g' > $@

% : %.cbl
	cobc -I include -std=ibm-strict -x $^

ASREA178 : ASREA003.cbl ASREA178.cbl

ASREA852 : ASREA852.cbl ASREA003.cbl

ASREA853 : ASREA853.cbl ASREA003.cbl
