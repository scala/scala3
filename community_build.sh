# COMMUNITY_BUILD.SH notes: 
#       (1) the main goal is to build several community-build projects to test the performance of scala 3 compiler 
#       (2) the name of the community-build projects need to be added to project_list 
#       (3) the output of building community-build projects can be found at output/project_name.log
#       (4) output/result_summary.log contain two lists of project names; one for those that are built successfully, and one for those that are not

#include all the community projects that need to be built in the following project_list
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

#set output to be the name of the directory that project.log and result_summary.log will be stored
output=output_1
#remove the output directory if it already exits and make a new one
rm -r "${output}"
mkdir "${output}"

project_success=()
project_fail=()

# run each project in the project_list; add the project to one of project_success or project_fail base on the exit_status
for project in "${project_list[@]}" 
do
    sbt "community-build/testOnly *${project}" &> "${output}/${project}.log"
    exit_status=$?
    if [ exit_status ]
    then
        project_success+=("${project}")
    else
        project_fail+=("${project}")
    fi
done

# generate output/result_summary.log which include information in project_success and project_fail
echo "${#project_success[@]} projects are successfully built:  " >> "${output}/result_summary.log"
for sproject in "${project_success[@]}" 
do
    echo -e "\t${sproject}" >> "${output}/result_summary.log"
done

echo -e "\n ${#project_fail[@]} projects fail to build:  " >> "${output}/result_summary.log"
for fproject in "${project_fail[@]}" 
do
    echo -e "\t${fproject} \n" >> "${output}/result_summary.log"
done

