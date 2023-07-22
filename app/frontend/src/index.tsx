import React from "react";
import ReactDOM from "react-dom/client";
import { BrowserRouter, Route, Routes } from "react-router-dom";
import { initializeIcons } from "@fluentui/react";

import "./index.css";

import Layout from "./pages/layout/Layout";
import Chat from "./pages/chat/Chat";
import Presentation from "./pages/presentation/Presentation";

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
                <Route path="/presentation/:name?" element={<Presentation />} />
                <Route path="/" element={<Layout />}>
                    <Route path="/" element={<Chat />} />
                    <Route path="qa" lazy={() => import("./pages/oneshot/OneShot")} />
                    <Route path="*" lazy={() => import("./pages/NoPage")} />
                </Route>
            </Routes>
        </BrowserRouter>
    </React.StrictMode>
);
