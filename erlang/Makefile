
REBAR=$(shell which rebar || echo ./rebar)
KAMAILIO_HOME=/home/sip-router/sip-router/
SCHEMA_HOME=$(KAMAILIO_HOME)/lib/srdb1/schema
DB_CATALOG = $(KAMAILIO_HOME)/doc/stylesheets/dbschema_k/catalog.xml
#KAM_TABLE_FILES := $(patsubst kamailio-%.xml,%,$(wildcard $(KAMAILIO_HOME)//lib/srdb1/schema/kamailio-*.xml))
KAM_TABLE_FILES := $(wildcard $(SCHEMA_HOME)/kamailio-*.xml)
XSLTPROC = /usr/bin/xsltproc
XSLTPROC_FLAGS = --xinclude

DB_SCHEMA=apps/db_mock/src/schema.erl
DB_SCHEMA_HRL=apps/db_mock/lib/schema.hrl
DB_ERLANG_XSL = db_erlang_mock.xsl
#all: deps dirs compile
all: deps compile

./rebar:
	erl -noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"https://github.com/downloads/basho/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

#dirs:
#	-@mkdir -p priv/tmp

db_erlang_mock.xml: $(KAM_TABLE_FILES) Makefile
	echo '<?xml version="1.0" encoding="UTF-8"?>' >db_erlang_mock.xml
	echo '<!DOCTYPE database PUBLIC "-//kamailio.org//DTD DBSchema V1.1//EN"' >>db_erlang_mock.xml
	echo '  "http://kamailio.org/pub/kamailio/dbschema/dtd/1.1/dbschema.dtd" [' >>db_erlang_mock.xml
	echo '  <!ENTITY % entities SYSTEM "entities.xml">' >>db_erlang_mock.xml
	echo '  %entities;' >>db_erlang_mock.xml
	echo '  ]>' >>db_erlang_mock.xml
	echo '  <foo xmlns:xi="http://www.w3.org/2001/XInclude">' >>db_erlang_mock.xml
	echo '      <name>temporaty agregated file for db_erlng</name>' >>db_erlang_mock.xml
	for FILE in $(KAM_TABLE_FILES); do echo "<xi:include href=\"$$FILE\" parse=\"xml\" xpointer=\"xpointer(/database)\"/>" >>db_erlang_mock.xml; done
	echo '</foo>' >>db_erlang_mock.xml

$(DB_SCHEMA): db_erlang_mock.xml db_erlang_mock.xsl
	XML_CATALOG_FILES=$(DB_CATALOG) $(XSLTPROC) $(XSLTPROC_FLAGS) \
	--path $(SCHEMA_HOME) \
	--stringparam dest "$(DB_SCHEMA)" \
	--stringparam hrldest "$(DB_SCHEMA_HRL)" \
	--stringparam db "db_erlang" \
	--debug \
	$(DB_ERLANG_XSL) db_erlang_mock.xml

compile: $(REBAR) $(DB_SCHEMA)
	$(REBAR) compile

clean: $(REBAR)
	$(REBAR) clean

deps: $(REBAR)
	$(REBAR) get-deps

.PHONY: deps
