'use strict';

console.log('app starting');

require('./Main.purs').main();

if (module.hot) {
  module.hot.accept();
}

