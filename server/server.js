const fetch = require('node-fetch');
const Bot = require('./bot.js');

async function startServer() {
    // Setup Token
    console.log('Checking token...')
    const token = process.env.TELEGRAM_TOKEN;
    const check = await isValidToken(token);
    if (check.error) {
        console.error(`The token '${token}' is invalid. Explanation:`)
        console.error(check.error);
        process.exit(1);
    }
    console.log(`Token valid for bot '${check.ok.first_name}'.`)
    const baseUrl = getBaseUrl(token);

    // Setup Elm
    const bot = Bot.Elm.Main.init();
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
    });

    // Run
    console.info('Bot started.')
    let offset = 0;

    while (true) {
        console.log(`Fetching updates starting with id ${offset}...`);
        const res = await fetch(
            baseUrl + 'getUpdates',
            {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ offset }),
            }
        );
        const json = await res.json();
        const updates = json.result;
        console.log('Received updates:');
        console.log(updates);

        const newOffset = await handleUpdates(updates);
        offset = newOffset ? newOffset : offset;

        await new Promise(resolve => {
            const delay = 0;
            setTimeout(resolve, delay);
        });
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
}

async function isValidToken(token) {
    const res = await fetch(getBaseUrl(token) + 'getMe');
    const json = await res.json();
    if (!json.ok) {
        return { error: json.description };
    } else {
        const user = json.result;
        if (!user.is_bot) {
            return { error: "Is not a bot." }
        } else {
            return { ok: user }
        }
    }

}

function getBaseUrl(token) {
    return `https://api.telegram.org/bot${token}/`;
}

startServer().catch(console.error);
