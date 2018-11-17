const fetch = require('node-fetch');

global.XMLHttpRequest = require('xhr2');
const Bot = require('./bot.js');

async function startServer() {
    // Setup Token
    console.log('Checking token...')
    const { user, token } = await getValidToken();
    console.log(`Token valid for bot '${user.first_name}'.`)
    const baseUrl = getBaseUrl(token);

    // Setup Elm
    const bot = Bot.Elm.Main.init();
    bot.ports.error.subscribe(function (errorMessage) {
        console.error(errorMessage);
    });
    bot.ports.sendMessage.subscribe(function (message) {
        console.log('\nSending message:');
        console.log(message);
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
        console.log(`\nFetching updates starting with id ${offset}...`);
        const res = await fetch(
            baseUrl + 'getUpdates',
            {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ offset }),
            }
        );
        const json = await res.json();
        if (json.ok) {
            const updates = json.result;
            console.log('\nReceived updates:');
            console.log(updates);

            const newOffset = await handleUpdates(updates);
            offset = newOffset ? newOffset : offset;

            await new Promise(resolve => {
                const delay = 0;
                setTimeout(resolve, delay);
            });
        } else {
            console.error('Error fetching updates:');
            console.error(json.description);
            process.exit(2);
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
}

async function getValidToken() {
    const tokenName = 'TELEGRAM_TOKEN';
    const token = process.env[tokenName];
    if (!token) {
        cancelWithError(`No token seems to be provided in the environment variable '${tokenName}'.`);
    }
    const res = await fetch(getBaseUrl(token) + 'getMe');
    const json = await res.json();
    if (!json.ok) {
        cancelWithError(json.description, token);
    } else {
        const user = json.result;
        return { user, token }
    }

    function cancelWithError(error, token) {
        console.error(`Could not verify the token${token ? " '" + token + "'" : ''}.`);
        console.error('Explanation:');
        console.error(error);
        process.exit(1);
    }
}

function getBaseUrl(token) {
    return `https://api.telegram.org/bot${token}/`;
}

startServer().catch(console.error);
