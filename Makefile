GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
NC := \033[0m

ROOT_PATH = $(PWD)
PROJECT_PATH = ${ROOT_PATH}/h42n42

all: build exec

build:
	@echo "${YELLOW}Building up h42n42...${NC}"
	cd ${PROJECT_PATH} && dune build
	@echo "${GREEN}h42n42 successfully built!\n${NC}"

exec: 
	@echo "${YELLOW}Starting the h42n42 app...${NC}"
	@echo "${YELLOW}Press Ctrl+C to stop${NC}"
	@cd ${PROJECT_PATH} && trap 'echo "${RED}Stopping server...${NC}"; exit 0' INT && dune exec -- python3 -m http.server 8000


clean:
	@echo "${YELLOW}Cleaning up h42n42...${NC}"
	cd ${PROJECT_PATH} && dune clean
	@echo "${GREEN}h42n42 project clean!\n${NC}"

re: clean all

.PHONY: build exec clean re
	