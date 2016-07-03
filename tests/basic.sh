# basic.sh -- check cuirass basic behavior
# Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>

# This file is part of Cuirass.

# Cuirass is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Cuirass is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Cuirass.  If not, see <http://www.gnu.org/licenses/>.

db_file=${testbuilddir}/test.db
spec_file=${testsrcdir}/hello-subset.scm

trap 'rm -f "$db_file"' EXIT

cuirass --database $db_file --one-shot $spec_file
