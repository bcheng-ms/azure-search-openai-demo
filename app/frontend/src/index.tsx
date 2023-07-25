import React from "react";
import ReactDOM from "react-dom/client";
import { BrowserRouter, Route, Routes } from "react-router-dom";
import { initializeIcons } from "@fluentui/react";

import "./index.css";

import Layout from "./pages/layout/Layout";
import Chat from "./pages/chat/Chat";
import GetStarted from "./pages/getstarted/GetStarted";
import Presentation from "./pages/presentation/Presentation";
import OneShot from "./pages/oneshot/OneShot";
import NoPage from "./pages/NoPage";
initializeIcons();

// const MainRouter = createBrowserRouter([
//     {
//         path: "/presentation/:name?",
//         element: <Presentation />
//     },
//     {
//         path: "/",
//         element: <Layout />,
//         children: [
//             {
//                 index: true,
//                 element: <Chat />
//             },
//             {
//                 path: "qa",
//                 lazy: () => import("./pages/oneshot/OneShot")
//             },
//             {
//                 path: "*",
//                 lazy: () => import("./pages/NoPage")
//             }
//         ]
//     }
// ]);

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
    <React.StrictMode>
        <BrowserRouter>
            <Routes>
                <Route path="/presentation/:name/:company" element={<Presentation />} />
                <Route path="/" element={<Layout />}>
                    <Route path="/" element={<GetStarted />} />
                    <Route path=":company/qa" element={<OneShot />} />
                    <Route path=":company" element={<Chat />} />
                    <Route path="*" element={<NoPage />} />
                </Route>
            </Routes>
        </BrowserRouter>
    </React.StrictMode>
);
