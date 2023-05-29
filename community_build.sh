# COMMUNITY_BUILD.SH notes: 
#       (1) the main goal is to build several community-build projects to test the performance of scala 3 compiler 
#       (2) the name of the community-build projects need to be added to project_list 
#       (3) the output of building community-build projects can be found at output/output_project_name.log
#       (4) output/result_summary.log contain two lists of project names; one for those that are built successfully, and one for those that are not
#       (5) the error message found in output/output_project_name.log are stored in output/error_project_name.log
#       (6) output/error_summary_count contain a table summarizing the # of error found in each output/output_project_name.log
#       (7) output/error_summary_message.log conatin a summary of all the errors found accross building all the projects

#include all the community projects that need to be built in the following project_list
project_list_1=("intent")
project_list=("intent"
            "scalacheck"
            "scalatest" 
            "scalatestplus-scalacheck"
            "scalatestplus-junit" 
            "scalatestplus-testng" 
            "scala-xml" 
            "scalap"
            "betterfiles" 
            "ScalaPB" 
            "minitest" 
            "fastparse" 
            "stdLib213" 
            "shapeless" 
            "xml-interpolator" 
            "effpi" 
            "sconfig" 
            "zio" 
            "munit" 
            "scodec-bits"
            "scodec" 
            "scala-parser-combinators" 
            "dotty-cps-async" 
            "scalaz" 
            "endpoints4s" 
            "cats-effect-3" 
            "scala-parallel-collections" 
            "scala-collection-compat"
            "scala-java8-compat" 
            "verify"  
            "discipline"  
            "discipline-munit" 
            "discipline-specs2" 
            "simulacrum-scalafix"  
            "cats" 
            "cats-mtl" 
            "coop" 
            "Equal" 
            "FingerTree"
            "Log" 
            "Model" 
            "Numbers" 
            "Serial" 
            "AsyncFile" 
            "Span" 
            "scala-stm" 
            "Lucre" 
            "izumi-reflect" 
            "perspective" 
            "akka" 
            "Monocle" 
            "protoquill" 
            "onnx-scala" 
            "play-json"
            "munit-cats-effect" 
            "scalacheck-effect" 
            "fs2"  
            "libretto"  
            "jackson-module-scala"  
            "specs2" 
            "spire" 
            "http4s" 
            "parboiled2"
        )

#set the current folder name
current_dir=test_community_build

#set output to be the name of the directory that project.log and result_summary.log will be stored
output=output_no_change

#remove the output directory if it already exits and make a new one
rm -r "${output}"
mkdir "${output}"

#set error to be the name of the directory that project.error and error_summary will be stored
error=error_no_change

#remove the error  directory if it already exits and make a new one
rm -r "${error}"
mkdir "${error}"

#create two array for storing the name of project that fail and success
project_success=()
project_fail=()

#print the header of error_summary
printf "%-30s %-10s\n" "Project Name" "# or Errors (explicit_nulls)" >> "${error}/error_summary_count"

summarize_error() {
     # write all the error messages to project.error
    grep "\[error\] .*" "${current_dir}/${output}/$1.log" >> "${current_dir}/${error}/error_$1.log"
    echo -e "\n !!! Error found for project $1: " >> "${current_dir}/${error}/error_summary_message.log"
    grep "\[error\] .*" "${current_dir}/${output}/$1.log" >> "${current_dir}/${error}/error_summary_message.log"

    # write the error # to error_summary
    error_line=$(grep -E -m 1 "\[error\] [[:alpha:]]+ errors found" "${current_dir}/${output}/$1.log")
    if [ -z "${error_line}"]; then
        error_line=$(grep -E -m 1 "\[error\] [[:alpha:]]+ error found" "${current_dir}/${output}/$1.log")
    fi
    error_count=$(echo "$error_line" | cut -d' ' -f2 | grep .)

    printf "%-30s %-10s\n" "$1" "${error_count}" >> "${current_dir}/${error}/error_summary_count"
}

cd ..
#run each project in the project_list; add the project to one of project_success or project_fail base on the exit_status
for project in "${project_list[@]}" 
do
    #build the community project
    sbt "community-build/testOnly *${project}" &> "${current_dir}/${output}/${project}.log"
    exit_status=$?
    #exit_status=1
    if [ exit_status == 0 ]; then
        project_success+=("${project}")
    else
        project_fail+=("${project}")
    fi
    summarize_error "${project}"
done

# generate output/result_summary.log which include information in project_success
echo "${#project_success[@]} projects are successfully built:  " >> "${current_dir}/${output}/result_summary.log"
for sproject in "${project_success[@]}" 
do
    echo -e "\t${sproject}" >> "${current_dir}/${output}/result_summary.log"
done

# generate output/result_summary.log which include information in project_fail
echo -e "\n${#project_fail[@]} projects fail to build:  " >> "${current_dir}/${output}/result_summary.log"
for fproject in "${project_fail[@]}" 
do
    echo -e "\t${fproject}" >> "${current_dir}/${output}/result_summary.log"
done

