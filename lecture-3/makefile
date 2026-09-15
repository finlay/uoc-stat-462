IMAGE := lecture3

docker:
	docker build -t $(IMAGE) .


local:
	docker run -it --rm -w /work -v $$PWD:/work \
    --net=host \
    -e DISPLAY=$$DISPLAY \
    -e RUN= \
    -v /tmp/.X11-unix/X0:/tmp/.X11-unix/X0 \
    -e HOME=$${HOME} \
    -u $$(id -u):$$(id -g) $(IMAGE) bash
