module SlackTestData exposing (convListJson, teamInfoJson, userInfoJson, userListJson)


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
