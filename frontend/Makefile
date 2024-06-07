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

all: ui.h

ui.h: ui.js
	@echo -n 'uijs = return "' > $@
	@sed -E 's/\t/  /g; s/\\/\\\\/g; s/"/\\"/g; s/\n/\\n/g;' $< >> $@
	@echo '"' >> $@

ui.js: UI.elm Api.elm Logo.elm elm.json
ifdef JASSO_DEV
	@elm make --debug $< --output=$@ > /dev/null
else
	@elm make --optimize $< --output=elm.js > /dev/null
	@uglifyjs elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $@
	@$(RM) -f elm.js
endif

clean:
	@$(RM) -fr ui.h *.js elm-stuff

.PHONY: all clean
