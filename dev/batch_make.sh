#!/bin/bash

BMAKE=Makefile.base
MAKE_CMD=make
MAKE_DPATH=makefiles
IND="  "

# Unicode for colored ✔ and ✘
RED="\033[0;31m"
GRN="\033[0;32m"
NC="\033[0m" # No Color
BLT="${RED}\u2718${NC}"
CHK="${GRN}\u2714${NC}"

# sed command to change option. e.g., OPTION = VALUE
change_option() {
  FILE=$1
  OPT=$2
  VAL=$3

  # NOTE: Hard to find a seperator character 
  REGEX=("s!\(^.*\)${OPT}\([ ]*\)=\([ ]*\).*!\1${OPT}\2=\3${VAL}!g")
  sed -i -e "${REGEX[@]}" $FILE
}

# sed command to uncomment flag, e.g., # FLAG_1 = DSEDIMENT => FLAG_1 = DSEDIMENT
uncomment_flag() {
  FILE=$1
  FLAG=$2

  REGEX=("s!\(^.*\)#\(.*\)=\( *\)-D${FLAG}!\1\2=\3-D${FLAG}!g")
  sed -i -e "${REGEX[@]}" $FILE
}

# Comment all flags
comment_flags() {
	REGEX=("s!^[^#]*FLAG_\(.*\)=\([ ]*\)-D\(.*\)!# FLAG_\1=\2-D\3!")
	sed -i -e "${REGEX[@]}" $1
}

# Make FUNWAVE exectuable with optional FLAGs
gen_make_single() {

  IS_PARALLEL=$1
  NAME=$2
  FLAGS=${@:3:${#@}}


  # Generating file names
  if [ "$IS_PARALLEL" == "T" ]; then
    EXEC_NAME="funwave-${NAME}"
    LFILE="${MAKE_DPATH}/${NAME}.log"
  else
    EXEC_NAME="funwave-serial-${NAME}"
    LFILE="${MAKE_DPATH}/serial-${NAME}.log"
  fi

  # Creating Makefile for exectuable based on base Makefile
  NMAKE="$MAKE_DPATH/Makefile.$NAME"
  cp $BMAKE $NMAKE

  # Updating executable name and flags
  change_option $NMAKE 'EXEC' "$EXEC_NAME"
  for FLAG in ${FLAGS[@]}; do
    uncomment_flag $NMAKE $FLAG
  done

  # Cleaning and building exectuable
  make -f $NMAKE clean > $LFILE 2>&1
  make -f $NMAKE >> $LFILE 2>&1

  # Outputting success/error message
  if [ $? -eq 0 ]; then
    SEXE=$(( $SEXE + 1))
    echo -e "$IND[$CHK] ${EXEC_NAME} built successfully."
  else
    echo -e "$IND[$BLT] ${EXEC_NAME} failed to built, see ${LFILE}."
  fi

  TEXE=$(( $TEXE + 1))
}

# Builds FUNWAVE exectuables for either serial or parallel mode
gen_make_batch() {

  IS_PARALLEL=$1
  
  # Counters
  SEXE=0; TEXE=0

  # Setting up parallel/serial option 
  if [ "$IS_PARALLEL" == "T" ]; then
    echo "-------------------------------------"
    echo "Building parallel versions of FUNWAVE"
    echo "-------------------------------------"
    change_option $BMAKE "PARALLEL" "true"
  else 
    echo "-----------------------------------"
    echo "Building serial versions of FUNWAVE"
    echo "-----------------------------------"
    change_option $BMAKE "PARALLEL" "false"
  fi

  # Building different versions of FUNWAVE
  gen_make_single $IS_PARALLEL "central" 
  gen_make_single $IS_PARALLEL "sediment" SEDIMENT 
  gen_make_single $IS_PARALLEL "sediment-vessel" SEDIMENT VESSEL

  # Summary output
  if [ "$SEXE" -eq "$TEXE" ]; then MRK=${CHK}; else MRK=${BLT}; fi 
  echo ""
  echo -e "$IND[$MRK] $SEXE/$TEXE executables built successfully." 
}


if [ "$#" -ne 1 ]; then
	echo "ERROR: Script takes one argument which is the base Makefile!"
	exit 1
fi

# Copying base Makefile
cp $1 $BMAKE

# Changing build paths and removing flags
change_option $BMAKE "FUNWAVE_DIR" ".."
change_option $BMAKE "WORK_DIR" "../funwave-work"
comment_flags $BMAKE


# Cleaning up log and Makefile directory
mkdir -p $MAKE_DPATH
rm $MAKE_DPATH/*

# Making parallel executables
gen_make_batch 'T'
echo ""
# Making serial executables 
gen_make_batch 'F'



