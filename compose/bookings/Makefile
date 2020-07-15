IMAGE=bookings-api
VERSION=1.0
CONTAINER=api
TAR_NAME=../tars/bookings_api.tar
NETWORK=smst_network

build:
	@echo 'Building Docker image'
	DOCKER_BUILDKIT=1 docker build -t ${IMAGE}:${VERSION} .

cleanup:
	@echo 'Removing untagged images'
	@docker rmi $$(docker images -f "dangling=true" -q)

run:
	@echo 'Running Container from API image'
	@docker run --rm \
	-d \
	-v $$(pwd)/data:/root/bookings_api/data \
	-p 6060:6060 \
	--name ${CONTAINER} \
	--net ${NETWORK} \
	${IMAGE}:${VERSION}

ts:
	docker run -it \
	-v $$(pwd)/data:/root/bookings/data \
	--entrypoint /bin/bash \
	--rm \
	--name ${CONTAINER}
	${IMAGE}:${VERSION}

stop:
	@echo 'Stopping API'
	@docker stop ${CONTAINER}

stopall:
	@echo 'Stopping all containers'	
	@docker stop $$(docker ps -a -q)

removeall:
	@echo 'Removing all containers'
	@docker container rm $$(docker ps -a -q)

view:
	@docker ps -a
	
save:
	@docker save -o ${TAR_NAME} ${IMAGE}:${VERSION}
