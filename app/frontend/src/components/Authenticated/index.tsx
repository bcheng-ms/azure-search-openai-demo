import { useOktaAuth } from "@okta/okta-react";
import { toRelativeUrl } from "@okta/okta-auth-js";
import { useEffect } from "react";

export default function Authenticated({ success, loading }) {
    const { oktaAuth, authState } = useOktaAuth();

    useEffect(() => {
        if (authState?.isAuthenticated === false) {
            const originalUri = toRelativeUrl(globalThis.location.href, globalThis.location.origin);
            oktaAuth.setOriginalUri(originalUri);
            oktaAuth.signInWithRedirect();
        }
    }, [oktaAuth, authState?.isAuthenticated]);

    return authState?.isAuthenticated === true ? success : loading;
}
