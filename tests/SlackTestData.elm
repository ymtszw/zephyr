module SlackTestData exposing (botInfoJson, convListJson, conversationHistoryJson, teamInfoJson, userInfoJson, userListJson)


botInfoJson : String
botInfoJson =
    """
{
    "ok": true,
    "bot": {
        "id": "BDE9XGSD7",
        "deleted": false,
        "name": "GitHub",
        "updated": 1539659476,
        "app_id": "A8GBNUWU8",
        "user_id": "UDE61U7LG",
        "icons": {
            "image_36": "https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-12-19/288981919427_f45f04edd92902a96859_36.png",
            "image_48": "https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-12-19/288981919427_f45f04edd92902a96859_48.png",
            "image_72": "https://slack-files2.s3-us-west-2.amazonaws.com/avatars/2017-12-19/288981919427_f45f04edd92902a96859_72.png"
        }
    }
}
    """


conversationHistoryJson : String
conversationHistoryJson =
    """
{
    "ok": true,
    "messages": [
        {
            "bot_id": "BESP6DK8A",
            "type": "message",
            "text": "",
            "user": "UESV1NECD",
            "ts": "1545051759.001200",
            "attachments": [
                {
                    "fallback": "[ymtszw/zephyr] Issue closed by ymtszw",
                    "pretext": "Issue closed by ymtszw",
                    "title": "#24 Consider deduplication of Items",
                    "footer": "<https://github.com/ymtszw/zephyr|ymtszw/zephyr>",
                    "id": 1,
                    "title_link": "https://github.com/ymtszw/zephyr/issues/24",
                    "footer_icon": "https://assets-cdn.github.com/favicon.ico",
                    "ts": 1542021767,
                    "color": "cb2431",
                    "mrkdwn_in": [
                        "text"
                    ]
                }
            ]
        },
        {
            "type": "message",
            "text": "",
            "files": [
                {
                    "id": "FETS7RDPH",
                    "created": 1544689009,
                    "timestamp": 1544689009,
                    "name": "localhost_8000_ (3).png",
                    "title": "localhost_8000_ (3).png",
                    "mimetype": "image/png",
                    "filetype": "png",
                    "pretty_type": "PNG",
                    "user": "UEPUX32MD",
                    "editable": false,
                    "size": 227816,
                    "mode": "hosted",
                    "is_external": false,
                    "external_type": "",
                    "is_public": true,
                    "public_url_shared": false,
                    "display_as_bot": false,
                    "username": "",
                    "url_private": "https://files.slack.com/files-pri/TEP3UKAJH-FETS7RDPH/localhost_8000___3_.png",
                    "url_private_download": "https://files.slack.com/files-pri/TEP3UKAJH-FETS7RDPH/download/localhost_8000___3_.png",
                    "thumb_64": "https://files.slack.com/files-tmb/TEP3UKAJH-FETS7RDPH-146ccb7f79/localhost_8000___3__64.png",
                    "thumb_80": "https://files.slack.com/files-tmb/TEP3UKAJH-FETS7RDPH-146ccb7f79/localhost_8000___3__80.png",
                    "thumb_360": "https://files.slack.com/files-tmb/TEP3UKAJH-FETS7RDPH-146ccb7f79/localhost_8000___3__360.png",
                    "thumb_360_w": 88,
                    "thumb_360_h": 360,
                    "thumb_480": "https://files.slack.com/files-tmb/TEP3UKAJH-FETS7RDPH-146ccb7f79/localhost_8000___3__480.png",
                    "thumb_480_w": 117,
                    "thumb_480_h": 480,
                    "thumb_160": "https://files.slack.com/files-tmb/TEP3UKAJH-FETS7RDPH-146ccb7f79/localhost_8000___3__160.png",
                    "thumb_720": "https://files.slack.com/files-tmb/TEP3UKAJH-FETS7RDPH-146ccb7f79/localhost_8000___3__720.png",
                    "thumb_720_w": 175,
                    "thumb_720_h": 720,
                    "thumb_800": "https://files.slack.com/files-tmb/TEP3UKAJH-FETS7RDPH-146ccb7f79/localhost_8000___3__800.png",
                    "thumb_800_w": 195,
                    "thumb_800_h": 800,
                    "thumb_960": "https://files.slack.com/files-tmb/TEP3UKAJH-FETS7RDPH-146ccb7f79/localhost_8000___3__960.png",
                    "thumb_960_w": 234,
                    "thumb_960_h": 960,
                    "thumb_1024": "https://files.slack.com/files-tmb/TEP3UKAJH-FETS7RDPH-146ccb7f79/localhost_8000___3__1024.png",
                    "thumb_1024_w": 249,
                    "thumb_1024_h": 1024,
                    "image_exif_rotation": 1,
                    "original_w": 346,
                    "original_h": 1421,
                    "permalink": "https://zephyr-dev.slack.com/files/UEPUX32MD/FETS7RDPH/localhost_8000___3_.png",
                    "permalink_public": "https://slack-files.com/TEP3UKAJH-FETS7RDPH-0a0b6ef453"
                }
            ],
            "upload": true,
            "user": "UEPUX32MD",
            "display_as_bot": false,
            "ts": "1544689013.000600"
        },
        {
            "type": "message",
            "subtype": "bot_message",
            "text": "<https://deadspin.com/pacers-fans-play-the-most-infuriating-game-of-tic-tac-t-1831064210|Pacers Fans Play The Most Infuriating Game Of Tic-Tac-Toe In Human History>\\nBe warned: What you are about to see will trouble your dreams for years to come. What the fuck. Two Indiana Pacers fans squared off in a game of tic-tac-toe, and the results are just fucking mind-boggling.\\n<https://deadspin.com/pacers-fans-play-the-most-infuriating-game-of-tic-tac-t-1831064210|Read more...>",
            "ts": "1544686635.000400",
            "username": "Kotaku",
            "icons": {
                "image_36": "https://a.slack-edge.com/66f9/img/services/rss_36.png",
                "image_48": "https://a.slack-edge.com/66f9/img/services/rss_48.png",
                "image_72": "https://a.slack-edge.com/205a/img/services/rss_72.png"
            },
            "bot_id": "BEST8J877",
            "attachments": [
                {
                    "service_name": "Deadspin",
                    "title": "Pacers Fans Play The Most Infuriating Game Of Tic-Tac-Toe In Human History",
                    "title_link": "https://deadspin.com/pacers-fans-play-the-most-infuriating-game-of-tic-tac-t-1831064210",
                    "text": "Be warned: What you are about to see will trouble your dreams for years to come. What the fuck. Two Indiana Pacers fans squared off in a game of tic-tac-toe, and the results are just fucking mind-boggling.",
                    "fallback": "Deadspin: Pacers Fans Play The Most Infuriating Game Of Tic-Tac-Toe In Human History",
                    "image_url": "https://i.kinja-img.com/gawker-media/image/upload/s--N4ciN-ev--/c_fill,fl_progressive,g_center,h_900,q_80,w_1600/qt1fswsdifj4fq1zeg7r.jpg",
                    "from_url": "https://deadspin.com/pacers-fans-play-the-most-infuriating-game-of-tic-tac-t-1831064210",
                    "image_width": 444,
                    "image_height": 250,
                    "image_bytes": 151733,
                    "service_icon": "https://i.kinja-img.com/gawker-media/image/upload/s--iIvh_25i--/c_fill,fl_progressive,g_center,h_200,q_80,w_200/rnxqtvv6advgidzfs6am.png",
                    "id": 1,
                    "original_url": "https://deadspin.com/pacers-fans-play-the-most-infuriating-game-of-tic-tac-t-1831064210"
                }
            ]
        },
        {
            "type": "message",
            "subtype": "thread_broadcast",
            "text": "threaded but globally visible!",
            "user": "UEPUX32MD",
            "ts": "1544685094.000100",
            "thread_ts": "1544668605.000400",
            "root": {
                "client_msg_id": "195b9713-c56b-4e1a-8ad4-b0b725a8c1cd",
                "type": "message",
                "text": "threaded",
                "user": "UEPUX32MD",
                "ts": "1544668605.000400",
                "thread_ts": "1544668605.000400",
                "reply_count": 3,
                "reply_users_count": 1,
                "latest_reply": "1544685094.000100",
                "reply_users": [
                    "UEPUX32MD"
                ],
                "replies": [
                    {
                        "user": "UEPUX32MD",
                        "ts": "1544668615.000500"
                    },
                    {
                        "user": "UEPUX32MD",
                        "ts": "1544668618.000700"
                    },
                    {
                        "user": "UEPUX32MD",
                        "ts": "1544685094.000100"
                    }
                ],
                "subscribed": true,
                "last_read": "1544685094.000100"
            },
            "client_msg_id": "a4608c9e-3f8c-4dc1-8c1f-78ba1a7c46cb"
        },
        {
            "bot_id": "BES1GFZA4",
            "type": "message",
            "text": "",
            "user": "UESV1NECD",
            "ts": "1544682681.001300",
            "attachments": [
                {
                    "text": "This channel will get notifications from <https://github.com/ymtszw/zephyr|ymtszw/zephyr> for:\\n`issues`, `pulls`, `deployments`, `statuses`, `public`, `commits:all`, `releases`",
                    "footer": "<https://github.com/integrations/slack#configuration|Learn More>",
                    "id": 1,
                    "color": "24292f",
                    "mrkdwn_in": [
                        "text",
                        "footer"
                    ],
                    "fallback": "This channel will get notifications from <https://github.com/ymtszw/zephyr|ymtszw/zephyr> for:\\n`issues`, `pulls`, `deployments`, `statuses`, `public`, `commits:all`, `releases`"
                }
            ]
        },
        {
            "type": "message",
            "text": "/github subscribe ymtszw/zephyr commits:all",
            "user": "UEPUX32MD",
            "ts": "1544682681.001200"
        },
        {
            "bot_id": "BES1GFZA4",
            "type": "message",
            "text": "",
            "user": "UESV1NECD",
            "ts": "1544682553.001100",
            "attachments": [
                {
                    "text": "Subscribed to <https://github.com/ymtszw/zephyr|ymtszw/zephyr>",
                    "id": 1,
                    "color": "24292f",
                    "fallback": "Subscribed to <https://github.com/ymtszw/zephyr|ymtszw/zephyr>"
                }
            ]
        },
        {
            "type": "message",
            "text": "<https://github.com/kubernetes-incubator/external-storage/issues/953>",
            "files": [
                {
                    "id": "FEU55K71U",
                    "created": 1544680387,
                    "timestamp": 1544680387,
                    "name": "localhost_8000_.png",
                    "title": "localhost_8000_.png",
                    "mimetype": "image/png",
                    "filetype": "png",
                    "pretty_type": "PNG",
                    "user": "UEPUX32MD",
                    "editable": false,
                    "size": 1413565,
                    "mode": "hosted",
                    "is_external": false,
                    "external_type": "",
                    "is_public": true,
                    "public_url_shared": false,
                    "display_as_bot": false,
                    "username": "",
                    "url_private": "https://files.slack.com/files-pri/TEP3UKAJH-FEU55K71U/localhost_8000_.png",
                    "url_private_download": "https://files.slack.com/files-pri/TEP3UKAJH-FEU55K71U/download/localhost_8000_.png",
                    "thumb_64": "https://files.slack.com/files-tmb/TEP3UKAJH-FEU55K71U-e192b21afd/localhost_8000__64.png",
                    "thumb_80": "https://files.slack.com/files-tmb/TEP3UKAJH-FEU55K71U-e192b21afd/localhost_8000__80.png",
                    "thumb_360": "https://files.slack.com/files-tmb/TEP3UKAJH-FEU55K71U-e192b21afd/localhost_8000__360.png",
                    "thumb_360_w": 360,
                    "thumb_360_h": 353,
                    "thumb_480": "https://files.slack.com/files-tmb/TEP3UKAJH-FEU55K71U-e192b21afd/localhost_8000__480.png",
                    "thumb_480_w": 480,
                    "thumb_480_h": 471,
                    "thumb_160": "https://files.slack.com/files-tmb/TEP3UKAJH-FEU55K71U-e192b21afd/localhost_8000__160.png",
                    "thumb_720": "https://files.slack.com/files-tmb/TEP3UKAJH-FEU55K71U-e192b21afd/localhost_8000__720.png",
                    "thumb_720_w": 720,
                    "thumb_720_h": 707,
                    "thumb_800": "https://files.slack.com/files-tmb/TEP3UKAJH-FEU55K71U-e192b21afd/localhost_8000__800.png",
                    "thumb_800_w": 800,
                    "thumb_800_h": 786,
                    "thumb_960": "https://files.slack.com/files-tmb/TEP3UKAJH-FEU55K71U-e192b21afd/localhost_8000__960.png",
                    "thumb_960_w": 960,
                    "thumb_960_h": 943,
                    "thumb_1024": "https://files.slack.com/files-tmb/TEP3UKAJH-FEU55K71U-e192b21afd/localhost_8000__1024.png",
                    "thumb_1024_w": 1024,
                    "thumb_1024_h": 1005,
                    "image_exif_rotation": 1,
                    "original_w": 1494,
                    "original_h": 1467,
                    "permalink": "https://zephyr-dev.slack.com/files/UEPUX32MD/FEU55K71U/localhost_8000_.png",
                    "permalink_public": "https://slack-files.com/TEP3UKAJH-FEU55K71U-5486b18619"
                }
            ],
            "upload": true,
            "user": "UEPUX32MD",
            "display_as_bot": false,
            "ts": "1544680393.000100"
        },
        {
            "client_msg_id": "a542b6a1-e494-4110-9eb4-85dfcc5f4641",
            "type": "message",
            "text": "<@USLACKBOT> /help",
            "user": "UEPUX32MD",
            "ts": "1544672975.003600"
        },
        {
            "type": "message",
            "text": "",
            "files": [
                {
                    "id": "FETNETEUX",
                    "created": 1544670675,
                    "timestamp": 1544681294,
                    "name": "Post",
                    "title": "Post",
                    "mimetype": "text/plain",
                    "filetype": "space",
                    "pretty_type": "ポスト",
                    "user": "UEPUX32MD",
                    "editable": true,
                    "size": 697,
                    "mode": "space",
                    "is_external": false,
                    "external_type": "",
                    "is_public": true,
                    "public_url_shared": false,
                    "display_as_bot": false,
                    "username": "",
                    "url_private": "https://files.slack.com/files-pri/TEP3UKAJH-FETNETEUX/Post",
                    "url_private_download": "https://files.slack.com/files-pri/TEP3UKAJH-FETNETEUX/download/Post",
                    "permalink": "https://zephyr-dev.slack.com/files/UEPUX32MD/FETNETEUX/Post",
                    "permalink_public": "https://slack-files.com/TEP3UKAJH-FETNETEUX-d9bb37beed",
                    "preview": "<document><p>Post allows composing rich text document within Slack.</p><p>They are considered one of files from API perspective.</p><p></p><p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p></document>",
                    "updated": 1544681294,
                    "editor": "UEPUX32MD",
                    "last_editor": "UEPUX32MD",
                    "state": "inactive"
                }
            ],
            "upload": false,
            "user": "UEPUX32MD",
            "display_as_bot": false,
            "ts": "1544670757.003000"
        },
        {
            "type": "message",
            "text": "",
            "files": [
                {
                    "id": "FESHS3DH9",
                    "created": 1544670089,
                    "timestamp": 1544670089,
                    "name": "______.txt",
                    "title": "無題",
                    "mimetype": "text/plain",
                    "filetype": "text",
                    "pretty_type": "プレーンテキスト",
                    "user": "UEPUX32MD",
                    "editable": true,
                    "size": 44,
                    "mode": "snippet",
                    "is_external": false,
                    "external_type": "",
                    "is_public": true,
                    "public_url_shared": false,
                    "display_as_bot": false,
                    "username": "",
                    "url_private": "https://files.slack.com/files-pri/TEP3UKAJH-FESHS3DH9/______.txt",
                    "url_private_download": "https://files.slack.com/files-pri/TEP3UKAJH-FESHS3DH9/download/______.txt",
                    "permalink": "https://zephyr-dev.slack.com/files/UEPUX32MD/FESHS3DH9/______.txt",
                    "permalink_public": "https://slack-files.com/TEP3UKAJH-FESHS3DH9-ce823c07bd",
                    "edit_link": "https://zephyr-dev.slack.com/files/UEPUX32MD/FESHS3DH9/______.txt/edit",
                    "preview": "type AuthorId\\n    = UAuthorId UserId\\n   | BAuthorId BotId\\n\\ntype BotId =",
                    "preview_highlight": "",
                    "lines": 18,
                    "lines_more": 13,
                    "preview_is_truncated": false
                }
            ],
            "upload": true,
            "user": "UEPUX32MD",
            "display_as_bot": false,
            "ts": "1544670089.002700"
        },
        {
            "client_msg_id": "800e6ef5-2820-452d-ae41-f1dc0abbc3a2",
            "type": "message",
            "text": "<https://github.com/ymtszw/zephyr>",
            "user": "UEPUX32MD",
            "ts": "1544668729.002500",
            "attachments": [
                {
                    "service_name": "GitHub",
                    "title": "ymtszw/zephyr",
                    "title_link": "https://github.com/ymtszw/zephyr",
                    "text": "Contribute to ymtszw/zephyr development by creating an account on GitHub.",
                    "fallback": "GitHub: ymtszw/zephyr",
                    "from_url": "https://github.com/ymtszw/zephyr",
                    "thumb_url": "https://avatars1.githubusercontent.com/u/4507126?s=400&v=4",
                    "thumb_width": 250,
                    "thumb_height": 250,
                    "service_icon": "https://a.slack-edge.com/bfaba/img/unfurl_icons/github.png",
                    "id": 1,
                    "original_url": "https://github.com/ymtszw/zephyr"
                }
            ]
        },
        {
            "client_msg_id": "21d03db5-bbaa-4e78-981e-badbae761ab0",
            "type": "message",
            "text": "Markdown\\n```\\ncode : String\\ncode =\\n    \\"Hey!\\"\\n```",
            "user": "UEPUX32MD",
            "ts": "1544668678.002100",
            "edited": {
                "user": "UEPUX32MD",
                "ts": "1544668692.000000"
            }
        },
        {
            "type": "message",
            "text": "file",
            "files": [
                {
                    "id": "FESFH7KDJ",
                    "created": 1544668652,
                    "timestamp": 1544668652,
                    "name": "elm.jpg",
                    "title": "elm.jpg",
                    "mimetype": "image/jpeg",
                    "filetype": "jpg",
                    "pretty_type": "JPEG",
                    "user": "UEPUX32MD",
                    "editable": false,
                    "size": 3450,
                    "mode": "hosted",
                    "is_external": false,
                    "external_type": "",
                    "is_public": true,
                    "public_url_shared": false,
                    "display_as_bot": false,
                    "username": "",
                    "url_private": "https://files.slack.com/files-pri/TEP3UKAJH-FESFH7KDJ/elm.jpg",
                    "url_private_download": "https://files.slack.com/files-pri/TEP3UKAJH-FESFH7KDJ/download/elm.jpg",
                    "thumb_64": "https://files.slack.com/files-tmb/TEP3UKAJH-FESFH7KDJ-659e5100ff/elm_64.jpg",
                    "thumb_80": "https://files.slack.com/files-tmb/TEP3UKAJH-FESFH7KDJ-659e5100ff/elm_80.jpg",
                    "thumb_360": "https://files.slack.com/files-tmb/TEP3UKAJH-FESFH7KDJ-659e5100ff/elm_360.jpg",
                    "thumb_360_w": 88,
                    "thumb_360_h": 88,
                    "thumb_160": "https://files.slack.com/files-tmb/TEP3UKAJH-FESFH7KDJ-659e5100ff/elm_160.jpg",
                    "image_exif_rotation": 1,
                    "original_w": 88,
                    "original_h": 88,
                    "permalink": "https://zephyr-dev.slack.com/files/UEPUX32MD/FESFH7KDJ/elm.jpg",
                    "permalink_public": "https://slack-files.com/TEP3UKAJH-FESFH7KDJ-42f79e45e7"
                }
            ],
            "upload": true,
            "user": "UEPUX32MD",
            "display_as_bot": false,
            "ts": "1544668654.001500"
        },
        {
            "client_msg_id": "4452cf08-3a4a-4947-ba59-bb25446f3c08",
            "type": "message",
            "text": "reaction",
            "user": "UEPUX32MD",
            "ts": "1544668627.001200",
            "reactions": [
                {
                    "name": "heart",
                    "users": [
                        "UEPUX32MD"
                    ],
                    "count": 1
                }
            ]
        },
        {
            "client_msg_id": "1c9b9be2-4900-4af8-838e-b8c6a7f97919",
            "type": "message",
            "text": "日本語",
            "user": "UEPUX32MD",
            "ts": "1544668624.001000"
        },
        {
            "client_msg_id": "195b9713-c56b-4e1a-8ad4-b0b725a8c1cd",
            "type": "message",
            "text": "threaded",
            "user": "UEPUX32MD",
            "ts": "1544668605.000400",
            "thread_ts": "1544668605.000400",
            "reply_count": 3,
            "reply_users_count": 1,
            "latest_reply": "1544685094.000100",
            "reply_users": [
                "UEPUX32MD"
            ],
            "replies": [
                {
                    "user": "UEPUX32MD",
                    "ts": "1544668615.000500"
                },
                {
                    "user": "UEPUX32MD",
                    "ts": "1544668618.000700"
                },
                {
                    "user": "UEPUX32MD",
                    "ts": "1544685094.000100"
                }
            ],
            "subscribed": true,
            "last_read": "1544685094.000100"
        },
        {
            "client_msg_id": "6407b18b-2afd-4c2b-a084-d5f62c386aa7",
            "type": "message",
            "text": "test",
            "user": "UEPUX32MD",
            "ts": "1544668593.000200"
        },
        {
            "user": "UEPUX32MD",
            "type": "message",
            "subtype": "channel_join",
            "ts": "1544269050.000200",
            "text": "<@UEPUX32MD>さんがチャンネルに参加しました"
        }
    ],
    "has_more": false,
    "pin_count": 0
}
    """


userInfoJson : String
userInfoJson =
    """
{
  "user": {
    "has_2fa": false,
    "updated": 1530495226,
    "is_app_user": false,
    "tz_label": "日本標準時",
    "tz": "Asia/Tokyo",
    "real_name": "Yu Matsuzawa",
    "color": "9f69e7",
    "deleted": false,
    "name": "yu.matsuzawa",
    "team_id": "T0950TCP9",
    "id": "U0950TCSK",
    "tz_offset": 32400,
    "profile": {
      "is_custom_image": true,
      "team": "T0950TCP9",
      "status_text_canonical": "",
      "image_1024": "https://avatars.slack-edge.com/2015-08-15/9170844512_6125e38289b3e8b41a94_512.jpg",
      "image_512": "https://avatars.slack-edge.com/2015-08-15/9170844512_6125e38289b3e8b41a94_512.jpg",
      "image_192": "https://avatars.slack-edge.com/2015-08-15/9170844512_6125e38289b3e8b41a94_192.jpg",
      "image_72": "https://avatars.slack-edge.com/2015-08-15/9170844512_6125e38289b3e8b41a94_72.jpg",
      "image_48": "https://avatars.slack-edge.com/2015-08-15/9170844512_6125e38289b3e8b41a94_48.jpg",
      "image_32": "https://avatars.slack-edge.com/2015-08-15/9170844512_6125e38289b3e8b41a94_32.jpg",
      "image_24": "https://avatars.slack-edge.com/2015-08-15/9170844512_6125e38289b3e8b41a94_24.jpg",
      "fields": null,
      "display_name_normalized": "yu.matsuzawa",
      "display_name": "yu.matsuzawa",
      "real_name_normalized": "Yu Matsuzawa",
      "real_name": "Yu Matsuzawa",
      "skype": "",
      "phone": "08051448946",
      "title": "",
      "status_text": "",
      "status_emoji": "",
      "status_expiration": 0,
      "avatar_hash": "6125e38289b3",
      "image_original": "https://avatars.slack-edge.com/2015-08-15/9170844512_6125e38289b3e8b41a94_original.jpg",
      "email": "ymtszw@gmail.com",
      "first_name": "Yu",
      "last_name": "Matsuzawa"
    },
    "is_admin": true,
    "is_owner": true,
    "is_primary_owner": true,
    "is_restricted": false,
    "is_ultra_restricted": false,
    "is_bot": false
  },
  "ok": true
}
    """


teamInfoJson : String
teamInfoJson =
    """
{
  "team": {
    "icon": {
      "image_default": true,
      "image_230": "https://a.slack-edge.com/bfaba/img/avatars-teams/ava_0020-230.png",
      "image_132": "https://a.slack-edge.com/66f9/img/avatars-teams/ava_0020-132.png",
      "image_102": "https://a.slack-edge.com/66f9/img/avatars-teams/ava_0020-102.png",
      "image_88": "https://a.slack-edge.com/66f9/img/avatars-teams/ava_0020-88.png",
      "image_68": "https://a.slack-edge.com/66f9/img/avatars-teams/ava_0020-68.png",
      "image_44": "https://a.slack-edge.com/66f9/img/avatars-teams/ava_0020-44.png",
      "image_34": "https://a.slack-edge.com/0180/img/avatars-teams/ava_0020-34.png"
    },
    "email_domain": "",
    "domain": "norafarm",
    "name": "norafarm",
    "id": "T0950TCP9"
  },
  "ok": true
}
    """


convListJson : String
convListJson =
    """
{
    "ok": true,
    "channels": [
        {
            "id": "CENNEBF6Y",
            "name": "zephyr",
            "is_channel": true,
            "is_group": false,
            "is_im": false,
            "created": 1544269050,
            "is_archived": false,
            "is_general": false,
            "unlinked": 0,
            "name_normalized": "zephyr",
            "is_shared": false,
            "parent_conversation": null,
            "creator": "UEPUX32MD",
            "is_ext_shared": false,
            "is_org_shared": false,
            "shared_team_ids": [
                "TEP3UKAJH"
            ],
            "pending_shared": [],
            "is_pending_ext_shared": false,
            "is_member": true,
            "is_private": false,
            "is_mpim": false,
            "topic": {
                "value": "",
                "creator": "",
                "last_set": 0
            },
            "purpose": {
                "value": "",
                "creator": "",
                "last_set": 0
            },
            "previous_names": [],
            "num_members": 1
        },
        {
            "id": "CENQCKDGB",
            "name": "general",
            "is_channel": true,
            "is_group": false,
            "is_im": false,
            "created": 1544269050,
            "is_archived": false,
            "is_general": true,
            "unlinked": 0,
            "name_normalized": "general",
            "is_shared": false,
            "parent_conversation": null,
            "creator": "UEPUX32MD",
            "is_ext_shared": false,
            "is_org_shared": false,
            "shared_team_ids": [
                "TEP3UKAJH"
            ],
            "pending_shared": [],
            "is_pending_ext_shared": false,
            "is_member": true,
            "is_private": false,
            "is_mpim": false,
            "topic": {
                "value": "",
                "creator": "UEPUX32MD",
                "last_set": 1544269050
            },
            "purpose": {
                "value": "",
                "creator": "UEPUX32MD",
                "last_set": 1544269050
            },
            "previous_names": [],
            "num_members": 1
        },
        {
            "id": "CEP3UKE0M",
            "name": "random",
            "is_channel": true,
            "is_group": false,
            "is_im": false,
            "created": 1544269050,
            "is_archived": false,
            "is_general": false,
            "unlinked": 0,
            "name_normalized": "random",
            "is_shared": false,
            "parent_conversation": null,
            "creator": "UEPUX32MD",
            "is_ext_shared": false,
            "is_org_shared": false,
            "shared_team_ids": [
                "TEP3UKAJH"
            ],
            "pending_shared": [],
            "is_pending_ext_shared": false,
            "is_member": true,
            "is_private": false,
            "is_mpim": false,
            "topic": {
                "value": "",
                "creator": "UEPUX32MD",
                "last_set": 1544269050
            },
            "purpose": {
                "value": "",
                "creator": "UEPUX32MD",
                "last_set": 1544269050
            },
            "previous_names": [],
            "num_members": 1
        },
        {
            "id": "GEP35AMM2",
            "name": "secrets",
            "is_channel": false,
            "is_group": true,
            "is_im": false,
            "created": 1544368956,
            "is_archived": false,
            "is_general": false,
            "unlinked": 0,
            "name_normalized": "secrets",
            "is_shared": false,
            "parent_conversation": null,
            "creator": "UEPUX32MD",
            "is_ext_shared": false,
            "is_org_shared": false,
            "shared_team_ids": [
                "TEP3UKAJH"
            ],
            "pending_shared": [],
            "is_pending_ext_shared": false,
            "is_member": true,
            "is_private": true,
            "is_mpim": false,
            "last_read": "1544368965.000400",
            "is_open": true,
            "topic": {
                "value": "",
                "creator": "",
                "last_set": 0
            },
            "purpose": {
                "value": "",
                "creator": "",
                "last_set": 0
            },
            "priority": 0
        },
        {
            "id": "DENTDAAN6",
            "created": 1544269049,
            "is_im": true,
            "is_org_shared": false,
            "user": "UEPUX32MD",
            "is_user_deleted": false,
            "priority": 0
        },
        {
            "id": "DEN5PTNKS",
            "created": 1544269049,
            "is_im": true,
            "is_org_shared": false,
            "user": "USLACKBOT",
            "is_user_deleted": false,
            "priority": 0
        }
    ],
    "response_metadata": {
        "next_cursor": ""
    }
}
    """


userListJson : String
userListJson =
    """
{
    "ok": true,
    "members": [
        {
            "id": "UEPUX32MD",
            "team_id": "TEP3UKAJH",
            "name": "ymtszw",
            "deleted": false,
            "color": "9f69e7",
            "real_name": "Yu Matsuzawa",
            "tz": "Asia/Tokyo",
            "tz_label": "日本標準時",
            "tz_offset": 32400,
            "profile": {
                "title": "",
                "phone": "",
                "skype": "",
                "real_name": "Yu Matsuzawa",
                "real_name_normalized": "Yu Matsuzawa",
                "display_name": "ymtszw",
                "display_name_normalized": "ymtszw",
                "status_text": "",
                "status_emoji": "",
                "status_expiration": 0,
                "avatar_hash": "ge927410e6bd",
                "email": "ymtszw@gmail.com",
                "first_name": "Yu",
                "last_name": "Matsuzawa",
                "image_24": "https://secure.gravatar.com/avatar/e927410e6bdbca8dae29a0e3c33e388a.jpg?s=24&d=https%3A%2F%2Fa.slack-edge.com%2F0180%2Fimg%2Favatars%2Fava_0021-24.png",
                "image_32": "https://secure.gravatar.com/avatar/e927410e6bdbca8dae29a0e3c33e388a.jpg?s=32&d=https%3A%2F%2Fa.slack-edge.com%2F66f9%2Fimg%2Favatars%2Fava_0021-32.png",
                "image_48": "https://secure.gravatar.com/avatar/e927410e6bdbca8dae29a0e3c33e388a.jpg?s=48&d=https%3A%2F%2Fa.slack-edge.com%2F3654%2Fimg%2Favatars%2Fava_0021-48.png",
                "image_72": "https://secure.gravatar.com/avatar/e927410e6bdbca8dae29a0e3c33e388a.jpg?s=72&d=https%3A%2F%2Fa.slack-edge.com%2F66f9%2Fimg%2Favatars%2Fava_0021-72.png",
                "image_192": "https://secure.gravatar.com/avatar/e927410e6bdbca8dae29a0e3c33e388a.jpg?s=192&d=https%3A%2F%2Fa.slack-edge.com%2F7fa9%2Fimg%2Favatars%2Fava_0021-192.png",
                "image_512": "https://secure.gravatar.com/avatar/e927410e6bdbca8dae29a0e3c33e388a.jpg?s=512&d=https%3A%2F%2Fa.slack-edge.com%2F7fa9%2Fimg%2Favatars%2Fava_0021-512.png",
                "status_text_canonical": "",
                "team": "TEP3UKAJH"
            },
            "is_admin": true,
            "is_owner": true,
            "is_primary_owner": true,
            "is_restricted": false,
            "is_ultra_restricted": false,
            "is_bot": false,
            "is_app_user": false,
            "updated": 1544269234,
            "has_2fa": false
        },
        {
            "id": "USLACKBOT",
            "team_id": "TEP3UKAJH",
            "name": "slackbot",
            "deleted": false,
            "color": "757575",
            "real_name": "slackbot",
            "tz": null,
            "tz_label": "アメリカ太平洋標準時",
            "tz_offset": -28800,
            "profile": {
                "title": "",
                "phone": "",
                "skype": "",
                "real_name": "slackbot",
                "real_name_normalized": "slackbot",
                "display_name": "slackbot",
                "display_name_normalized": "slackbot",
                "fields": null,
                "status_text": "",
                "status_emoji": "",
                "status_expiration": 0,
                "avatar_hash": "sv1444671949",
                "always_active": true,
                "first_name": "slackbot",
                "last_name": "",
                "image_24": "https://a.slack-edge.com/0180/img/slackbot_24.png",
                "image_32": "https://a.slack-edge.com/7f1a0/plugins/slackbot/assets/service_32.png",
                "image_48": "https://a.slack-edge.com/7f1a0/plugins/slackbot/assets/service_48.png",
                "image_72": "https://a.slack-edge.com/0180/img/slackbot_72.png",
                "image_192": "https://a.slack-edge.com/66f9/img/slackbot_192.png",
                "image_512": "https://a.slack-edge.com/1801/img/slackbot_512.png",
                "status_text_canonical": "",
                "team": "TEP3UKAJH"
            },
            "is_admin": false,
            "is_owner": false,
            "is_primary_owner": false,
            "is_restricted": false,
            "is_ultra_restricted": false,
            "is_bot": false,
            "is_app_user": false,
            "updated": 0
        }
    ],
    "cache_ts": 1544427561
}
    """
