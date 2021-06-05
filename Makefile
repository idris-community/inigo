include config.mk

all : inigo
.PHONY : bootstrap server inigo install static local-server deploy-init deploy test

define build_base_dep
	(cd Base/$(1) && idris2 --build Bootstrap.ipkg --build-dir ../../build)
endef

bootstrap :
	mkdir -p build
	$(call build_base_dep,Color)
	$(call build_base_dep,Extra)
	$(call build_base_dep,Fmt)
	$(call build_base_dep,IdrTest)
	$(call build_base_dep,Markdown)
	$(call build_base_dep,SemVar)
	$(call build_base_dep,Toml)
	$(MAKE) inigo

server :
	idris2 --build Server/InigoServer.ipkg --cg node

inigo :
	idris2 --build Inigo.ipkg --cg node
	echo '#!/usr/bin/env node' | cat - build/exec/inigo > temp && mv temp build/exec/inigo
	chmod +x build/exec/inigo
	@echo "Built \"build/exec/inigo\""

install : inigo
	cp build/exec/inigo $(INSTALL_DIR)
	chmod +x $(INSTALL_DIR)/inigo

static :
	env SKIP_EXT=true node Server/InigoStatic/localize.js Server/InigoStatic/Pages Server/InigoStatic/Local/pages.json
	node Server/InigoStatic/localize.js Server/InigoStatic/Static Server/InigoStatic/Local/static.json

local-server : server static
	cloudworker --debug \
		--kv-file "pages=./Server/InigoStatic/Local/pages.json" \
		--kv-file "static=./Server/InigoStatic/Local/static.json" \
		--kv-file "index=./Server/InigoStatic/Local/index.json" \
		--kv-file "packages=./Server/InigoStatic/Local/packages.json" \
		--kv-file "deps=./Server/InigoStatic/Local/deps.json" \
		--kv-file "archives=./Server/InigoStatic/Local/archives.json" \
		--kv-file "readme=./Server/InigoStatic/Local/readme.json" \
		--kv-file "accounts=./Server/InigoStatic/Local/accounts.json" \
		--kv-file "sessions=./Server/InigoStatic/Local/sessions.json" \
		build/exec/inigo-server

deploy-init :
	terraform init -upgrade -var-file=${HOME}/InigoStatic.tfvars InigoStatic

deploy : static server
	terraform apply -var-file=${HOME}/InigoStatic.tfvars Server/InigoStatic

test :
	idris2 --find-ipkg Test/Suite.idr --cg node -x suite
