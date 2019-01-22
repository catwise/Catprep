#! /bin/tcsh -f 

set wrapperDir = $PWD
set startTime = `date +"%Y%m%d_%H%M%S"`
echo 
echo Wrapper Started at:
echo $startTime
echo
echo Version 1.0  
echo
echo This Wrapper will wrap around and run:
echo 1\) catprep 

#check hyphenated argument
@ i = 0
set rsyncSet = "false"
set gsaSet = "false"
set withinMode2 = "false"  # Used to determine if mode 3 is being called within mode 2
while ($i < $# + 1)
     #user input nameslist -nl argument
      if("$argv[$i]" == "-rsync") then
        echo Argument "-rsync" detected. Will rsync Tyto, Otus, and Athene.
        set rsyncSet = "true"
      endif
      if("$argv[$i]" == "-gsa") then
        echo Argument "-gsa" detected. Will call gsa in do-ab_gsa.tcsh
        set gsaSet = "true"
      endif
      if("$argv[$i]" == "-withinMode2") then
        echo Argument "-withinMode2" detected. 
        set withinMode2 = "true"
	echo $withinMode2
      endif
      @ i +=  1
end

#check mode and input arguments 
if ($# < 2) then
        #Error handling
        #Too many or too little arguments       
        echo ""
        echo "ERROR: not enough arguments:"
        echo Mode 2 call:
        echo './catprep_wrapper.tcsh 2 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version'
        echo Mode 3 call:
        echo './catprep_wrapper.tcsh 3 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version'
        echo
        echo Exiting...
        exit
#Mode2 List Mode
#TODO *** priority ***
#this list functionality is most important
# output is important
else if ($1 == 2) then
	set InputsList = $2
	set mdex_path = $3	
	set catalog_tile_name = $4
	set reject_file_name = $5
	set Version = $6
        
	echo Inputs list == $InputsList
	echo mdex_path == $mdex_path
        echo catalog_tile_name == $catalog_tile_name
        echo reject_file_name == $reject_file_name
	echo Version = $Version
	echo

        #if directories dont exist, throw error
        if(! -f $InputsList) then
                echo ERROR: Input List file $InputsList does not exist.
                echo
                echo Exiting...
                exit
        endif
        if(! -d $mdex_path) then
                echo ERROR: Input Path directory $mdex_path does not exist.
                echo
                echo Exiting...
                exit
        endif
	echo
	echo Going to Mode2
	echo
	goto Mode2
#Mode3 Single Tile Mode
else if ($1 == 3) then
	set RadecID = $2
	set mdex_path = $3	
	set catalog_tile_name = $4
	set reject_file_name = $5
	set Version = $6

	echo RadecID == $RadecID
	echo mdex_path == $mdex_path
        echo catalog_tile_name == $catalog_tile_name
        echo reject_file_name == $reject_file_name
	echo Version = $Version

        #if directories dont exist, throw error
        if(! -d $mdex_path) then
                echo ERROR: Input Path directory $mdex_path does not exist.
                echo
                echo Exiting...
                exit
        endif
        if(! -d $catalog_tile_name) then
                echo ERROR: Output Path directory $catalog_tile_name does not exist.
                echo
                echo Exiting...
                exit
        endif
        if(! -d $reject_file_name) then
                echo ERROR: Output Path directory $reject_file_name does not exist.
                echo
                echo Exiting...
                exit
        endif

	echo
	echo Going to Mode3
	echo
        goto Mode3
else
        #Error handling
        #option 2/3 not second parameter. program exits.
	echo
        echo "ERROR mode 2, or 3 not selected"
        echo "Mode 2 call:"
        echo './catprep_wrapper.tcsh 2 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version'
	echo "Mode 3 call:"
        echo './catprep_wrapper.tcsh 3 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version'	
	echo
        echo Exiting...
	exit
endif

#==============================================================================================================================

Mode2:
    
    foreach RadecID (`cat $InputsList`)    
        echo ===================================== - START catprep_wrapper wrapper loop iteration - ======================================
     
        echo "Current TileName == "${RadecID}
        echo Calling catprep_wrapper.tcsh Mode3 on ${RadecID}\: 
	if($gsaSet == "true") then
		if($rsyncSet == "true") then
			echo "${wrapperDir}/catprep_wrapper.tcsh 3 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version -rsync -gsa -withinMode2"
			(echo y | ${wrapperDir}/catprep_wrapper.tcsh 3 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version -rsync -gsa \
				-withinMode2) &
		else
			echo "${wrapperDir}/catprep_wrapper.tcsh 3 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version -gsa -withinMode2"
			(echo y | ${wrapperDir}/catprep_wrapper.tcsh 3 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version -gsa -withinMode2) &
		endif
	else
		if($rsyncSet == "true") then
			echo "${wrapperDir}/catprep_wrapper.tcsh 3 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version -rsync -withinMode2"
			(echo y | ${wrapperDir}/catprep_wrapper.tcsh 3 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version -rsync -withinMode2) &
		else
			echo "${wrapperDir}/catprep_wrapper.tcsh 3 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version -withinMode2"
			(echo y | ${wrapperDir}/catprep_wrapper.tcsh 3 ${RadecID} ${mdex_path} $catalog_tile_name $reject_file_name $Version -withinMode2) &
		endif
	
	endif
	
	set maxInParallel = 12
        if(`ps -ef | grep catprep_wrapper | wc -l` > $maxInParallel + 1) then
                echo  More than $maxInParallel catprep_wrapper processes, waiting...
                while(`ps -ef | grep catprep_wrapper | wc -l` > $maxInParallel + 1)
                        sleep 1
                        #echo IM WATING
                        #do nothing
                end
                echo  Done waiting
        endif
		echo
            
            echo ====================================== - END catprep_wrapper wrapper loop iteration - =======================================
    end

    #===============================================================================================================================================================

    #wait for background processes to finish
    wait
    echo catprep_wrapper wrapper finished
    echo
    goto Done

Mode3:	
	set RaRaRa = `echo $RadecID | awk '{print substr($0,0,3)}'`
	
	# Get the gzipped mdex file
	set mdex_name = `ls -tr1 ${mdex_path} | grep ${RadecID} | tail -1`
	set mdex_tile = ${mdex_path}/${mdex_name}
	echo "mdex_tile = ${mdex_tile}" 

	# Parse the ${mdex_tile}
	set tempSize = `basename $mdex_tile  | awk '{print length($0)}'`
        @ tempIndex = ($tempSize - 3 - 4) 
	set edited_mdexName = `basename $mdex_tile | awk -v endIndex=$tempIndex '{print substr($0,0,endIndex)}'`	
        @ tempIndex = ($tempIndex - 2 - 4 - 2) 
	set RestOfTablename = `basename $mdex_tile | awk -v endIndex=$tempIndex '{print substr($0,9,endIndex)}'`
	echo edited_mdexName = $edited_mdexName
	echo RestOfTablename = $RestOfTablename
	
	# Unzip the gzipped ${mdex_tile}
	echo Unzipping ${mdex_tile} to ${catalog_tile_name}/${RadecID}${RestOfTablename}.tbl
	gunzip -f -c -k ${mdex_tile} > ${catalog_tile_name}/${RadecID}${RestOfTablename}.tbl  # Unzip mdex file

	# Set mdex_name to unzipped mdex file
	set mdex_name = `ls -tr1 ${catalog_tile_name} | grep ${RadecID} | tail -1`
	set mdex_tile = ${catalog_tile_name}/${mdex_name}

	#echo "catprep -i $mdex_tile -c $catalog_tile_name -r $reject_file_name"
	#/Volumes/CatWISE1/jwf/src/catprep/catprep -i $mdex_tile -c $catalog_tile_name -r $reject_file_name
	echo "/Volumes/CatWISE1/jwf/src/catprep/catprep -i $mdex_tile -c $catalog_tile_name/${edited_mdexName}_cat_${Version} -r $reject_file_name/${edited_mdexName}_rj_${Version}"
	/Volumes/CatWISE1/jwf/src/catprep/catprep -i $mdex_tile -c $catalog_tile_name/${edited_mdexName}_cat_${Version} -r $reject_file_name/${edited_mdexName}_rj_${Version}

	goto Mode3_Done #gzip_done

Done:
echo catprep_wrapper Mode: ${1} Done
set endTime = `date '+%m/%d/%Y %H:%M:%S'`
echo
echo Wrapper Mode: ${1} Ended at:
echo $endTime
exit

#Done section for gzipping rsyncing
Mode3_Done:
echo catprep_wrapper on ${RadecID} Mode: ${1} Done
set endTime = `date '+%m/%d/%Y %H:%M:%S'`
#rm af file and rm original mdex table
echo "rm -f ${catalog_tile_name}/${RadecID}${RestOfTablename}.tbl"
rm -f ${catalog_tile_name}/${RadecID}${RestOfTablename}.tbl
echo
       #rsync step
	if($rsyncSet == "true") then
       #rsync output dir from Current server to other 2 servers (Tyto, Otus, Athene)
	set CatWISEDir = ${OutputPath}
        echo running rsync on tile $RadecID
        set currIP = `dig +short myip.opendns.com @resolver1.opendns.com`
        echo current IP = $currIP
        if($currIP == "137.78.30.21") then #Tyto
                set otus_CatWISEDir = `echo $CatWISEDir | sed 's/tyto1/otus5/g'`
                set athene_CatWISEDir = `echo $CatWISEDir | sed 's/tyto1/athene5/g'`
                echo You are on Tyto!

               #Transfer Tyto CatWISE/ dir to Otus
                echo rsync Tyto\'s $CatWISEDir ${RadecID}${RestOfTablename}_ab.tbl.gz, gsa-${RadecID}-af.txt, unwise-${RadecID}-msk.fit to Otus $otus_CatWISEDir
                ssh ${user}@137.78.80.75 "mkdir -p $otus_CatWISEDir"
                rsync -avur $CatWISEDir/ ${user}@137.78.80.75:$otus_CatWISEDir

               #Transfer Tyto CatWISE/ dir to Athene
                echo rsync Tyto\'s $CatWISEDir ${RadecID}${RestOfTablename}_ab.tbl.gz, gsa-${RadecID}-af.txt, unwise-${RadecID}-msk.fit to Athene $athene_CatWISEDir
                ssh ${user}@137.78.80.72 "mkdir -p $athene_CatWISEDir"
                rsync -avur $CatWISEDir/ ${user}@137.78.80.75:$athene_CatWISEDir


        else if($currIP == "137.78.80.75") then  #Otus
                set tyto_CatWISEDir = `echo $CatWISEDir | sed 's/otus5/tyto1/g'`
                set athene_CatWISEDir = `echo $CatWISEDir | sed 's/otus5/athene5/g'`
                echo You are on Otus!

               #Transfer Otus CatWISE/ dir to Tyto
                echo rsync Otus\'s $CatWISEDir${RadecID}${RestOfTablename}_ab.tbl.gz, gsa-${RadecID}-af.txt, unwise-${RadecID}-msk.fit to Tyto $tyto_CatWISEDir
                ssh ${user}@137.78.30.21 "mkdir -p $tyto_CatWISEDir"
                rsync -avur $CatWISEDir/ ${user}@137.78.80.75:$tyto_CatWISEDir

               #Transfer Otus CatWISE/ to Athene
                echo rsync Otus\'s $CatWISEDir ${RadecID}${RestOfTablename}_ab.tbl.gz, gsa-${RadecID}-af.txt, unwise-${RadecID}-msk.fit to Athene $athene_CatWISEDir
                ssh ${user}@137.78.80.72 "mkdir -p $athene_CatWISEDir"
                rsync -avur $CatWISEDir/ ${user}@137.78.80.75:$athene_CatWISEDir


        else if($currIP == "137.78.80.72") then #Athene
                set tyto_CatWISEDir = `echo $CatWISEDir | sed 's/athene5/tyto1/g'`
                set otus_CatWISEDir = `echo $CatWISEDir | sed 's/athene5/otus5/g'`
                echo You are on Athene!
               
	       #Transfer to Tyto
                echo rsync Athene\'s $CatWISEDir ${RadecID}${RestOfTablename}_ab.tbl.gz, gsa-${RadecID}-af.txt, unwise-${RadecID}-msk.fit to Tyto $tyto_CatWISEDir
		ssh ${user}@137.78.30.21 "mkdir -p $tyto_CatWISEDir"
		rsync -avur $CatWISEDir/ ${user}@137.78.80.75:$tyto_CatWISEDir

               #Transfer to Otus
                echo rsync Athene\'s $CatWISEDir ${RadecID}${RestOfTablename}_ab.tbl.gz, gsa-${RadecID}-af.txt, unwise-${RadecID}-msk.fit to Otus $otus_CatWISEDir
                ssh ${user}@137.78.80.75 "mkdir -p $otus_CatWISEDir"
                rsync -avur $CatWISEDir/ ${user}@137.78.80.75:$otus_CatWISEDir
        endif
        endif


echo
echo Wrapper Mode: ${1} Ended at:
echo $endTime
exit


#TODO save some lines! Simply set a variable == WARNING or ERROR. Then just do the same for both case (theres no need for that huge repeat) 
#program jumps here if a program returns an exit status 32(Warning) or 64(Error)
	######TODO: reduce redundencies in code
Failed:
echo exit status of ${failedProgram} for tile \[${RadecID}\]\: ${saved_status}
	set currIP = `dig +short myip.opendns.com @resolver1.opendns.com`
        echo current IP = $currIP
        if($currIP == "137.78.30.21") then #Tyto
		if($saved_status <= 32) then #status <= 32, WARNING 
			echo WARNING ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status} 	
			touch /Volumes/tyto2/ErrorLogsTyto/errorlog_IRSAWrapper_${startTime}.txt
			echo WARNING ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status}  >> /Volumes/tyto2/ErrorLogsTyto/errorlog_IRSAWrapper_${startTime}.txt 	
               		echo WARNING output to error log: /Volumes/tyto2/ErrorLogsTyto/errorlog_IRSAWrapper_${startTime}.txt
			if($rsyncSet == "true") then #rsync to other machines
	 	       	       #Transfer Tyto ErrorLogsTyto/ dir to Otus
               	 		echo rsync Tyto\'s /Volumes/tyto2/ErrorLogsTyto/ to Otus /Volumes/otus2/ErrorLogsTyto/
                		ssh ${user}@137.78.80.75 "mkdir -p /Volumes/otus2/ErrorLogsTyto/"
                		#rsync -avu /Volumes/tyto2/ErrorLogsTyto/ ${user}@137.78.80.75:/Volumes/otus2/ErrorLogsTyto/
	               	       #Transfer Tyto ErrorLogsTyto/ dir to Athene
        	        	echo rsync Tyto\'s /Volumes/tyto2/ErrorLogsTyto/ to Athene /Volumes/athene2/ErrorLogsTyto/ 
                		ssh ${user}@137.78.80.72 "mkdir -p /Volumes/athene2/ErrorLogsTyto/"
                		#rsync -avu  /Volumes/tyto2/ErrorLogsTyto/ ${user}@137.78.80.72:/Volumes/athene2/ErrorLogsTyto/ 
			endif
			echo Exiting wrapper...
			exit
		else if($saved_status > 32) then #status > 32, ERROR
			echo ERROR ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status} 
			touch /Volumes/tyto2/ErrorLogsTyto/errorlog_IRSAWrapper_${startTime}.txt
	                echo ERROR ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status}  >> /Volumes/tyto2/ErrorLogsTyto/errorlog_IRSAWrapper_${startTime}.txt
               		echo ERROR output to error log: /Volumes/tyto2/ErrorLogsTyto/errorlog_IRSAWrapper_${startTime}.txt
			if($rsyncSet == "true") then #rsync to other machines
	 	       	       #Transfer Tyto ErrorLogsTyto/ dir to Otus
               	 		echo rsync Tyto\'s /Volumes/tyto2/ErrorLogsTyto/ to Otus /Volumes/otus2/ErrorLogsTyto/
                		ssh ${user}@137.78.80.75 "mkdir -p /Volumes/otus2/ErrorLogsTyto/"
                		#rsync -avu /Volumes/tyto2/ErrorLogsTyto/ ${user}@137.78.80.75:/Volumes/otus2/ErrorLogsTyto/
	               	       #Transfer Tyto ErrorLogsTyto/ dir to Athene
        	        	echo rsync Tyto\'s /Volumes/tyto2/ErrorLogsTyto/ to Athene /Volumes/athene2/ErrorLogsTyto/ 
                		ssh ${user}@137.78.80.72 "mkdir -p /Volumes/athene2/ErrorLogsTyto/"
                		#rsync -avu  /Volumes/tyto2/ErrorLogsTyto/ ${user}@137.78.80.72:/Volumes/athene2/ErrorLogsTyto/ 
			endif
			echo Exiting wrapper...
			exit
		endif
	else if($currIP == "137.78.80.75") then  #Otus
		if($saved_status <= 32) then #status <= 32, WARNING
			echo WARNING ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status} 
			touch /Volumes/otus1/ErrorLogsOtus/errorlog_IRSAWrapper_${startTime}.txt
                	echo WARNING ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status}  >> /Volumes/otus1/ErrorLogsOtus/errorlog_IRSAWrapper_${startTime}.txt
               		echo WARNING output to error log: /Volumes/otus1/ErrorLogsOtus/errorlog_IRSAWrapper_${startTime}.txt
	
			if($rsyncSet == "true") then #rsync to other machines
	                       #Transfer Otus ErrorLogsOtus/ dir to Tyto
       		         	echo rsync Otus\'s /Volumes/otus1/ErrorLogsOtus/ to Tyto /Volumes/tyto1/ErrorLogsOtus/
       		         	ssh ${user}@137.78.30.21 "mkdir -p /Volumes/tyto1/ErrorLogsOtus/"
               		 	#rsync -avu /Volumes/otus1/ErrorLogsOtus/ ${user}@137.78.30.21:/Volumes/tyto1/ErrorLogsOtus/
            	   	       #Transfer Otus ErrorLogsOtus/ dir to Athene
            	    		echo rsync Otus\'s /Volumes/otus1/ErrorLogsOtus/ to Athene /Volumes/athene1/ErrorLogsOtus/
               		 	ssh ${user}@137.78.80.72 "mkdir -p /Volumes/athene1/ErrorLogsOtus/"
                		#rsync -avu /Volumes/otus1/ErrorLogsOtus/ ${user}@137.78.80.72:/Volumes/athene1/ErrorLogsOtus/
			endif
			echo Exiting wrapper...
			exit
		else if($saved_status > 32) then #status > 32, ERROR
                        echo ERROR ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status}
			touch /Volumes/otus1/ErrorLogsOtus/errorlog_IRSAWrapper_${startTime}.txt
                        echo ERROR ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status} >> /Volumes/otus1/ErrorLogsOtus/errorlog_IRSAWrapper_${startTime}.txt
                        echo ERROR output to error log: /Volumes/otus1/ErrorLogsOtus/errorlog_IRSAWrapper_${startTime}.txt
			if($rsyncSet == "true") then #rsync to other machines
	                       #Transfer Otus ErrorLogsOtus/ dir to Tyto
       		         	echo rsync Otus\'s /Volumes/otus1/ErrorLogsOtus/ to Tyto /Volumes/tyto1/ErrorLogsOtus/
       		         	ssh ${user}@137.78.30.21 "mkdir -p /Volumes/tyto1/ErrorLogsOtus/"
               		 	#rsync -avu /Volumes/otus1/ErrorLogsOtus/ ${user}@137.78.30.21:/Volumes/tyto1/ErrorLogsOtus/
            	   	       #Transfer Otus ErrorLogsOtus/ dir to Athene
            	    		echo rsync Otus\'s /Volumes/otus1/ErrorLogsOtus/ to Athene /Volumes/athene1/ErrorLogsOtus/
               		 	ssh ${user}@137.78.80.72 "mkdir -p /Volumes/athene1/ErrorLogsOtus/"
                		#rsync -avu /Volumes/otus1/ErrorLogsOtus/ ${user}@137.78.80.72:/Volumes/athene1/ErrorLogsOtus/
			endif
			echo Exiting wrapper...
			exit
                endif
	else if($currIP == "137.78.80.72") then  #Athene
                if($saved_status <= 32) then #status <= 32, WARNING
                        echo WARNING ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status}
			touch /Volumes/athene3/ErrorLogsAthene/errorlog_IRSAWrapper_${startTime}.txt
                        echo WARNING ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status} >> /Volumes/athene3/ErrorLogsAthene/errorlog_IRSAWrapper_${startTime}.txt
                        echo WARNING output to error log: /Volumes/athene3/ErrorLogsAthene/errorlog_IRSAWrapper_${startTime}.txt
                	
			if($rsyncSet == "true") then #rsync to other machines
                 	       #Transfer Athene ErrorLogsAthene/ dir to Tyto
                      	  	echo rsync Athene\'s /Volumes/athene3/ErrorLogsAthene/ to Tyto /Volumes/CatWISE3/ErrorLogsAthene/
                        	ssh ${user}@137.78.30.21 "mkdir -p /Volumes/CatWISE3/ErrorLogsAthene/"
                        	#rsync -avu /Volumes/athene3/ErrorLogsAthene/ ${user}@137.78.30.21:/Volumes/CatWISE3/ErrorLogsAthene/
              	               #Transfer Athene ErrorLogsTyto/ dir to Otus
                        	echo rsync Athene\'s /Volumes/athene3/ErrorLogsAthene/ to Otus /Volumes/otus3/ErrorLogsAthene/
                        	ssh ${user}@137.78.80.72 "mkdir -p /Volumes/otus3/ErrorLogsAthene/"
                        	#rsync -avu /Volumes/athene3/ErrorLogsAthene/ ${user}@137.78.80.72:/Volumes/otus3/ErrorLogsAthene/
                	endif
			echo Exiting wrapper...
			exit
                else if($saved_status > 32) then #status > 32, ERROR
                        echo ERROR ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status}
			touch /Volumes/athene3/ErrorLogsAthene/errorlog_IRSAWrapper_${startTime}.txt
                        echo ERROR ${failedProgram} on tile \[$RadecID\] exited with status ${saved_status} >> /Volumes/athene3/ErrorLogsAthene/errorlog_IRSAWrapper_${startTime}.txt
                        echo ERROR output to error log: /Volumes/athene3/ErrorLogsAthene/errorlog_IRSAWrapper_${startTime}.txt
                	if($rsyncSet == "true") then #rsync to other machines
                 	       #Transfer Athene ErrorLogsAthene/ dir to Tyto
                      	  	echo rsync Athene\'s /Volumes/athene3/ErrorLogsAthene/ to Tyto /Volumes/CatWISE3/ErrorLogsAthene/
                        	ssh ${user}@137.78.30.21 "mkdir -p /Volumes/CatWISE3/ErrorLogsAthene/"
                        	#rsync -avu /Volumes/athene3/ErrorLogsAthene/ ${user}@137.78.30.21:/Volumes/CatWISE3/ErrorLogsAthene/
              	               #Transfer Athene ErrorLogsTyto/ dir to Otus
                        	echo rsync Athene\'s /Volumes/athene3/ErrorLogsAthene/ to Otus /Volumes/otus3/ErrorLogsAthene/
                        	ssh ${user}@137.78.80.72 "mkdir -p /Volumes/otus3/ErrorLogsAthene/"
                        	#rsync -avu /Volumes/athene3/ErrorLogsAthene/ ${user}@137.78.80.72:/Volumes/otus3/ErrorLogsAthene/
                	endif
			echo Exiting wrapper...
			exit
                endif
	endif
	goto Mode3_Done
