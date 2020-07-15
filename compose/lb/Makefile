IMAGE=mynginx
NETWORK=smst_network

build:
	DOCKER_BUILDKIT=1 docker build -t ${IMAGE} .

save:
	docker save -o ../tars/mynginx.tar ${IMAGE}

run:
	docker run -p 80:80 \
	-d \
	--rm \
	--name test \
	--net ${NETWORK} \
	${IMAGE}

runit:
    docker run -p 80:80 \
    -it \
    --rm \
    --name test \
    --net ${NETWORK} \
    ${IMAGE} \
    /bin/bash

cleanup:
	docker rmi $$(docker images -f "dangling=true" -q)

certs:
	sudo openssl req -x509 \
	-nodes -days 365 \
	-newkey rsa:2048 \
	-keyout $$(pwd)/ssl/private/nginx-selfsigned.key \
	-out $$(pwd)/ssl/certs/nginx-selfsigned.crt

siege:
	siege -d1 -c10 -t1S -log http://localhost:80/bookings/plot?min_bookings=1000
