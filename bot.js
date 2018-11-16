const fetch = require('node-fetch');
const Echobot = require('./main.js');

const bot = Echobot.Elm.Main.init();
const token = process.env.TELEGRAM_TOKEN;
const baseUrl = `https://api.telegram.org/bot${token}/`;

bot.ports.error.subscribe(function (errorMessage) {
    console.error(errorMessage);
});
bot.ports.sendMessage.subscribe(function (message) {
    fetch(
        baseUrl + 'sendMessage',
        {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(message),
        }
    );
})

async function run(token) {
    console.info('Bot started.')
    let offset = 0;

    while (true) {
        console.log(`Fetching updates since id ${offset}...`);
        const res = await fetch(
            baseUrl + 'getUpdates',
            {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ offset }),
            }
        );
        const json = await res.json();
        console.log(json);
        const updates = json.result;
        console.log('Received updates:');
        console.log(updates);

        const newOffset = await handleUpdates(updates);
        offset = newOffset ? newOffset : offset;

        await new Promise(resolve => {
            const delay = 500;
            setTimeout(resolve, delay);
        });
    }
}

async function handleUpdates(updates) {
    const ids = updates.map(update => {
        bot.ports.incomingUpdate.send(update);
        return update.update_id;
    })

    if (ids.length) {
        return ids[ids.length - 1] + 1;
    } else {
        return null;
    }
}

run(token).catch(console.error);
