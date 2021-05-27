#!/usr/bin/env node
const prependFile = require('prepend-file')

prependFile.sync('bin/index.js', '#!/usr/bin/env node\n')
