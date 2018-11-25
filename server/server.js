const Elmegram = require('./elmegram.js');
const botPath = './bot.js';

const tokenName = 'TELEGRAM_TOKEN';
const token = process.env[tokenName];
Elmegram.startServer(token, botPath).catch(console.error);
