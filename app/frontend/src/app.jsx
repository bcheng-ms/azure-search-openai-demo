import React from 'react';
import { Route, useNavigate, Routes } from 'react-router-dom';
import { Security, SecureRoute, LoginCallback } from '@okta/okta-react';
import { OktaAuth, toRelativeUrl } from '@okta/okta-auth-js';

import Layout from "./pages/layout/Layout";
import Chat from "./pages/chat/Chat";
import GetStarted from "./pages/getstarted/GetStarted";
import Presentation from "./pages/presentation/Presentation";
import OneShot from "./pages/oneshot/OneShot";
import NoPage from "./pages/NoPage";
import Home from './home';
import Login from './pages/login';
import Protected from './pages/protected';
import config from './config';
import Authenticated from './components/Authenticated';

const oktaAuth = new OktaAuth(config.oidc);

const App = () => {
  const navigate = useNavigate();

  const customAuthHandler = () => {
    navigate('/login');
  };

  const restoreOriginalUri = async (_oktaAuth, originalUri) => {
    navigate(toRelativeUrl(originalUri || '', window.location.origin), {replace:true});
  };

  return (
    <div className="App">
      <Security
        oktaAuth={oktaAuth}
        onAuthRequired={customAuthHandler}
        restoreOriginalUri={restoreOriginalUri}
      >
        <Routes>
            <Route path="/presentation/:name/:company" element={<Presentation />} />
            {/* <Route path="/protected" element={<Layout />}>
                <Route path="/" element={<GetStarted />} />
                <Route path=":company/qa" element={<OneShot />} />
                <Route path=":company" element={<Chat />} />
                <Route path="*" element={<NoPage />} />
            </Route> */}
            <Route path="/protected" element={ <Authenticated success={<Layout />} />}>
                <Route index element={<Protected />} />
            </Route>
            <Route path="/login" render={() => <Login />} />
            <Route path="/login/callback" element={<LoginCallback />} />
            <Route path="/" element={<Layout />}>
                <Route path="/" element={<GetStarted />} />
                <Route path=":company/qa" element={<OneShot />} />
                <Route path=":company" element={<Chat />} />
                <Route path="*" element={<NoPage />} />
            </Route>
        </Routes>
    </Security>
    </div>
  );
};

export default App;
