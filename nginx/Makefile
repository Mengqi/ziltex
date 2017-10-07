###
# guide: http://anandmanisankar.com/posts/docker-container-nginx-node-redis-example/
# @author mengqi
#

user=zongmengqi
name=nginx
image_name=${user}/${name}
internal_port=80
external_port=80

build:
	docker build -t ${image_name} .
run:
	docker run -d --name ${name} -p ${external_port}:${internal_port} --link ziltex-node:ziltex-node ${image_name}
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

