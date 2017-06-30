# How to dockerize flowFramePlus
```
devtools::build()
```
This will produce a `.tar.gz` in the directory above the package directory
```
cp Dockerfile ../
cd ..
To install the AWS CLI and Docker and for more information on the steps below, visit the ECR documentation page.
1) Retrieve the docker login command that you can use to authenticate your Docker client to your registry: 
Note: 
If you receive an "Unknown options: --no-include-email" error, install the latest version of the AWS CLI. Learn more
aws ecr get-login --no-include-email --region us-east-1

2) Run the docker login command that was returned in the previous step. 
Note: 
If you are using Windows PowerShell, run the following command instead.
Invoke-Expression -Command (aws ecr get-login --no-include-email --region us-east-1)

3) Build your Docker image using the following command. For information on building a Docker file from scratch see the instructions here. You can skip this step if your image is already built:
docker build -t cytovas-ffp .

4) After the build completes, tag your image so you can push the image to this repository:
docker tag cytovas-ffp:latest 205853417430.dkr.ecr.us-east-1.amazonaws.com/cytovas-ffp:latest

5) Run the following command to push this image to your newly created AWS repository:
docker push 205853417430.dkr.ecr.us-east-1.amazonaws.com/cytovas-ffp:latest
```

To run from Amazon ECR
```
aws ecr get-login --no-include-email --region us-east-1
```
(run this login command)
```
docker run -it -v /projects:/projects 205853417430.dkr.ecr.us-east-1.amazonaws.com/cytovas-ffp /usr/local/lib/R/bin/R 
```
You can then
```
library("flowFramePlus")
```
