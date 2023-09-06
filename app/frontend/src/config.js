export default {
    oidc: {
        issuer: "https://dev-103479.okta.com/oauth2/default",
        clientId: "0oaeil0in7nnyNtqs4x7",
        scopes: ["openid", "profile", "email"],
        redirectUri: `${window.location.origin}/login/callback`
    },
    widget: {
        issuer: "https://dev-103479.okta.com/oauth2/default",
        clientId: "0oaeil0in7nnyNtqs4x7",
        redirectUri: `${window.location.origin}/login/callback`,
        scopes: ["openid", "profile", "email"]
    }
};
