# Jasso is Copyright (c) 2021-2024 Homebrew Holdings Pty Ltd.
# Contributors retain the copyright for their contributions.
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License along
# with this program.  If not, see <https://www.gnu.org/licenses/>.

JASSO_DEV := 1
ifeq ($(JASSO_DEV),1)
  CABALFLAGS := -v0 --enable-relocatable -fjasso_dev
  FRONTENDJS := UIJS.hs
else
  CABALFLAGS := -v1 --enable-relocatable
  FRONTENDJS := ui.js
endif
GHCRTS := -N

# # Tests are not included by default because the current testing framework relies on a particular infrastructure setup
#
# runtest: jasso
# 	@test/runtests.sh 2>&1 | changes .test.out
#
# test: jasso
#
# run: jasso
# 	@./jasso test/jasso.conf

all: build

build: jasso

install: jasso
	install -d -m755 $(DESTDIR)/usr/bin $(DESTDIR)/etc $(DESTDIR)/usr/share/doc/jasso
	install -m644 dist*/build/*/*/jasso-*/x/jasso/build/jasso/jasso $(DESTDIR)/usr/bin
	install -m644 doc/conf/jasso.conf $(DESTDIR)/etc/jasso.conf
	install -m644 -D doc/conf/sp.conf $(DESTDIR)/var/lib/jasso/saml/mysp.conf.sample
	install -m644 -D doc/conf/rp.conf $(DESTDIR)/var/lib/jasso/oidc/myrp.conf.sample
	install -m644 -D doc/conf/jasso.conf $(DESTDIR)/usr/share/doc/jasso/jasso.conf.sample
	install -m644 doc/conf/sp.conf $(DESTDIR)/usr/share/doc/jasso/sp.conf.sample
	install -m644 doc/conf/rp.conf $(DESTDIR)/usr/share/doc/jasso/rp.conf.sample

clean:
	@rm -rf -- jasso dist-* test/server.log test/cookies frontend/Api.elm
	@+$(MAKE) -sC frontend clean JASSO_DEV="$(JASSO_DEV)"

distclean: dist-clean
dist-clean: clean
	@rm -rf -- .test.out

jasso: jasso.cabal src/Main.hs src/JassoState.hs src/Certs.hs src/Config.hs src/ConfigTypes.hs src/Ldap.hs src/Logger.hs src/LogTypes.hs src/MkElmApi.hs src/Oidc.hs src/OidcTypes.hs src/Otp.hs src/Saml.hs src/Session.hs src/StdoutLogger.hs src/UI.hs src/UICommon.hs src/UIIncludes.cpphs src/UITypes.hs frontend/$(FRONTENDJS) frontend/piglet.ico
	cabal $(CABALFLAGS) build
	@ln -nfs "$$(ls -1 dist*/build/*/*/jasso-*/x/jasso/build/jasso/jasso | tail -1)" jasso

frontend/Api.elm: src/MkElmApi.hs src/UITypes.hs
	@cabal $(CABALFLAGS) build exe:mkelmapi
	@cabal $(CABALFLAGS) exec mkelmapi

frontend/$(FRONTENDJS): frontend/UI.elm frontend/Api.elm frontend/Logo.elm
	@+$(MAKE) -sC frontend JASSO_DEV="$(JASSO_DEV)"

.PHONY: runtest test run all build install clean dist-clean distclean
