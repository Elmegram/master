module Telegram.Test exposing (sendMessage)

import Telegram



-- HIGH-LEVEL


sendMessage : String -> Telegram.Update
sendMessage =
    makeMessage >> send



-- LOW-LEVEL


makeChat : Telegram.Chat
makeChat =
    { id = Telegram.makeTestId 1
    , type_ = Telegram.Private
    }


makeMessage : String -> Telegram.Message
makeMessage text =
    { message_id = Telegram.makeTestId 1
    , date = 1
    , chat = makeChat
    , text = text
    }


send : Telegram.Message -> Telegram.Update
send message =
    { update_id = Telegram.makeTestId 1
    , content = Telegram.MessageUpdate message
    }
