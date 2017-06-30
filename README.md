# How to dockerize flowFramePlus
```
devtools::build()
```
This will produce a `.tar.gz` in the directory above the package directory
```
cd ..
ln -s flowFramePlus/Dockerfile .
aws ecr get-login --no-include-email --region us-east-1
# run that
docker build -t cytovas-ffp .
docker tag cytovas-ffp:latest 205853417430.dkr.ecr.us-east-1.amazonaws.com/cytovas-ffp:latest
docker push 205853417430.dkr.ecr.us-east-1.amazonaws.com/cytovas-ffp:latest
```

# To run from Amazon ECR
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
