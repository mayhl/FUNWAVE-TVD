
printTitle () 
{   
    TITLE="  "${TITLE}"  " 
    LENGTH=${#TITLE}
    BANNER=$(printf "%-$((${LENGTH}+1))s" "*")
    BANNER=${BANNER// /-}
    BANNER=${BANNER:1:${LENGTH}}

    echo "" 
    echo ${BANNER}
    echo "${TITLE}"
    echo ${BANNER}		
}
 
