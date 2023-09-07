import React from "react";
import ReactDOM from "react-dom/client";
import App from "./app";
import { BrowserRouter } from "react-router-dom";
import { initializeIcons } from "@fluentui/react";

import "./index.css";

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
            <App />
        </BrowserRouter>
    </React.StrictMode>
);
