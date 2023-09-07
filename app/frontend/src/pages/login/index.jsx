import React from "react";
import { Navigate } from "react-router-dom";
import OktaSignInWidget from "../../components/OktaSignInWidget";
import { useOktaAuth } from "@okta/okta-react";

const Login = () => {
    const { oktaAuth, authState } = useOktaAuth();
    const onSuccess = tokens => {
        oktaAuth.handleLoginRedirect(tokens);
    };

    const onError = err => {
        console.log("Sign in error:", err);
    };

    if (!authState) {
        return <div>Loading ... </div>;
    }

    return authState.isAuthenticated ? <Navigate to={{ pathname: "/" }} /> : <OktaSignInWidget onSuccess={onSuccess} onError={onError} />;
};

export default Login;
