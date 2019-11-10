raw_cpy = $(wildcard raw/COPYBOOK.*)
cpy_files = $(patsubst raw/COPYBOOK.%,include/%.cpy,$(raw_cpy))

cbl_files = ASCOE500.cbl ASCOE515.cbl ASHMA440.cbl ASHMA550.cbl		\
ASHMA660.cbl ASHMA825.cbl ASHMA827.cbl ASHMA828.cbl ASHMA830.cbl	\
ASHMA831.cbl ASHMA832.cbl ASHMA836.cbl ASHMA839.cbl ASHMA840.cbl	\
ASHMA845.cbl ASHMA850.cbl ASHMA855.cbl ASHMA857.cbl ASHMA859.cbl	\
ASHMA921.cbl ASHMA922.cbl ASHMA923.cbl ASREA001.cbl ASREA002.cbl	\
ASREA003.cbl ASREA005.cbl ASREA007.cbl ASREA010.cbl ASREA018.cbl	\
ASREA105.cbl ASREA110.cbl ASREA151.cbl ASREA178.cbl ASREA352.cbl	\
ASREA740.cbl ASREA741.cbl ASREA742.cbl ASREA743.cbl ASREA744.cbl	\
ASREA745.cbl ASREA748.cbl ASREA841.cbl ASREA847.cbl ASREA852.cbl	\
ASREA853.cbl ASREA856.cbl ASREA859.cbl ASREA863.cbl ASREA864.cbl	\
ASREA866.cbl ASREA868.cbl ASREA869.cbl ASREA872.cbl ASREA874.cbl	\
ASREA880.cbl ASREA881.cbl ASREA909.cbl ASSRQ560.cbl ASSRQ561.cbl	\
CLREB020.cbl CLRTM356.cbl CLRTM749.cbl CLRTM750.cbl CLRTM751.cbl	\
CLRTM752.cbl CLRTM753.cbl CLRTM755.cbl CLRTM756.cbl CLRTM757.cbl	\
CLRTM759.cbl

ezt_files = easytrieve/ASHMA360.ezt easytrieve/ASHMA361.ezt	\
easytrieve/ASHMA441.ezt easytrieve/ASHMA856.ezt			\
easytrieve/ASREA004.ezt easytrieve/ASREA006.ezt			\
easytrieve/ASREA016.ezt easytrieve/ASREA037.ezt			\
easytrieve/ASREA828.ezt easytrieve/ASREA829.ezt			\
easytrieve/ASREA876.ezt easytrieve/ASREA896.ezt			\
easytrieve/ASREA897.ezt easytrieve/ASREA898.ezt			\
easytrieve/ASREA899.ezt easytrieve/CLRTM748.ezt			\
easytrieve/CLSRQ074.ezt

all : $(cpy_files) $(cbl_files) $(ezt_files)

include/%.cpy : raw/COPYBOOK.%
	cat $< | colrm 1 1 | \
            sed 's/++INCLUDE \(.*\)/COPY \1./g' | \
            sed 's///g' > $@

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
	cobc -I include -std=ibm -x $<

VSAMHMST : HMSTMSTR
	ASHMA440

MASTER : HOMESTD VETMAST MASTER
	ASHMA550

MASTER : COOP
	ASHMA660

SRFREEZE : SENIOR HOMEIN
	ASHMA825



ASCOEWRK : CONTCARD
	ASCOE500

ASCOEPRW : PARMCRD ASCOEWRK CARDSIN 
	ASCOE515

OUTFILE : ASCOEPRW EQUALFCT
	ASHMA855

FINDER : EQUALFCT PARAM CURRASS PRIORASS EXIRASS
	ASHMA850	

SNRFREZ : EQUALFCT PARAM CURASS PRIORASS EXPIRASS
	ASHMA857

MASTER : MASTER COEFILE
	ASHMA831
