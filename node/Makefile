###
# guide: 1) https://nodejs.org/en/docs/guides/nodejs-docker-webapp/
#        2) http://anandmanisankar.com/posts/docker-container-nginx-node-redis-example/
# @author mengqi
#

user=zongmengqi
name=ziltex-node
image_name=${user}/${name}
internal_port=8080
external_port=8080

build:
	docker build -t ${image_name} .
run:
	docker run -d --name ${name} -p ${external_port}:${internal_port} --link redis:redis ${image_name}
stop:
	docker stop $$(docker ps -a -q --filter ancestor=${image_name} --format="{{.ID}}")
rm:
	docker rm $$(docker ps -a -q --filter ancestor=${image_name} --format="{{.ID}}")
clean:
	make stop
	make rm
test:
	curl -Is localhost:${external_port}
logs:
	docker logs $$(docker ps -a -q --filter ancestor=${image_name} --format="{{.ID}}")

