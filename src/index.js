'use strict';

console.log('app starting');

require('../output/Main').main();

if (module.hot) {
  module.hot.accept();
}

