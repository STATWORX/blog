#!/bin/bash
echo ""
echo "This program deploys the Hotel Bookings API Backend to a previously created Docker-Machine VM of your choice"
echo ""
#check if docker-machine is installed
docker-machine --version
if [[ $? -ne 0 ]]; then
	echo "Please install docker-machine."
	exit 1
fi
echo ""
echo "The following virtual machines are available:"
echo ""
docker-machine ls
echo ""
n=0
until [[ $n -gt 2 ]];
do
	# get user input
	echo "Enter name of VM:"
	echo ""
	read VM_NAME
	echo ""
	docker-machine ls -q | grep -w $VM_NAME
	if [ $? -ne 0 ]; then
		echo "VM not found, try again."
		n=$(($n+1))
	else
		docker-machine start $VM_NAME && break
	fi
done
echo ""
echo "IP Address:"
docker-machine ip $VM_NAME
echo ""
echo "Setting environment variables"
eval $(docker-machine env $VM_NAME)
echo ""
echo "Available docker images:"
docker images
echo ""
echo "Starting application stack ..."
docker-compose up -d --scale api=3
echo ""
echo "Active containers:"
docker ps -a
echo ""
echo "Press any button to shut down the application stack"
read SHUTDOWN_STACK
if [ $? -eq 0 ]; then 
	docker-compose down
fi
echo ""
echo "Press any button to shut down the VM"
read SHUTDOWN_VM
if [ $? -eq 0 ]; then 
	docker-machine stop $VM_NAME
fi