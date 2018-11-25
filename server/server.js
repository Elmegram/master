// Import Elmegram before bot, in order to fix Elm's dependency on Browser APIs.
const Elmegram = require('elmegram.js');
const Bot = require('./bot.js');

const tokenName = 'TELEGRAM_TOKEN';
const token = process.env[tokenName];
Elmegram.startPolling(token, Bot).catch(console.error);
