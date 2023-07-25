import React from "react";
import { useParams } from "react-router-dom";
import styles from "./Presentation.module.css";
import ReactDOM from "react-dom";
import {
    FlexBox,
    Heading,
    SpectacleLogo,
    UnorderedList,
    CodeSpan,
    OrderedList,
    ListItem,
    Appear,
    Slide,
    Deck,
    Text,
    Grid,
    Box,
    Image,
    CodePane,
    MarkdownSlide,
    MarkdownSlideSet,
    Notes,
    SlideLayout
} from "spectacle";

import { AzureTemplate } from "./Template";
import logo from "./images/microsoft-logo.png";
import Slide2 from "./images/Slide2.jpg";
import Slide3 from "./images/Slide3.jpg";
import Slide4 from "./images/Slide4.jpg";
import Slide5 from "./images/Slide5.jpg";
import Slide6 from "./images/Slide6.jpg";
import Slide7 from "./images/Slide7.jpg";
import Slide8 from "./images/Slide8.jpg";
import Slide9 from "./images/Slide9.jpg";
import Slide10 from "./images/Slide10.jpg";
import Slide11 from "./images/Slide11.jpg";
import Slide12 from "./images/Slide12.jpg";
import DemoSlide from "./images/Slide13.jpg";
import Slide14 from "./images/Slide14.jpg";
import Slide15 from "./images/Slide15.jpg";
import Slide16 from "./images/Slide16.jpg";
import Slide17 from "./images/Slide17.jpg";
// SPECTACLE_CLI_THEME_START
const theme = {
    fonts: {
        header: '"Open Sans Condensed", Helvetica, Arial, sans-serif',
        text: '"Open Sans Condensed", Helvetica, Arial, sans-serif'
    },
    colors: {
        primary: "#0078d4",
        secondary: "#000000"
    }
};
// SPECTACLE_CLI_THEME_END

const SlideFragments = () => (
    <>
        <Slide>
            <Text>This is a slide fragment.</Text>
        </Slide>
        <Slide>
            <Text>This is also a slide fragment.</Text>
            <Appear>
                <Text>This item shows up!</Text>
            </Appear>
            <Appear>
                <Text>This item also shows up!</Text>
            </Appear>
        </Slide>
    </>
);

const template = <AzureTemplate />;

const Presentation = () => {
    let { name = "", company = "Northwind" } = useParams();
    return (
        <Deck theme={theme} template={template}>
            <Slide backgroundColor="#FFF">
                <FlexBox height="100%" flexDirection="column" alignItems="flex-start">
                    <FlexBox paddingTop={0} flex={1} alignItems="flex-start">
                        <Image src={logo} width={100} />
                    </FlexBox>
                    <FlexBox alignItems="flex-start" flex={1}>
                        <Heading margin="0px" color="primary" fontSize="h2">
                            OpenAI on Azure
                        </Heading>
                    </FlexBox>
                    <FlexBox alignItems="flex-start" flex={1}>
                        <Heading margin="0px 32px" color="secondary" fontSize="h3" textAlign="left">
                            {name}
                            <br />
                            Cloud Solution Architect
                        </Heading>
                    </FlexBox>
                </FlexBox>
                <Notes>
                    OpenAI on Azure
                    <ol>
                        <li>This presentation shows you how AI and Cognitive Services works together.</li>
                        <li>It shows you what AI can and cannot do.</li>
                        <li>It addresses the security and privacy concerns.</li>
                    </ol>
                </Notes>
            </Slide>
            <Slide backgroundImage={`url(${Slide2})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide3})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide4})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide5})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide6})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide7})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide8})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide9})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide10})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide11})`} backgroundOpacity={1}>
                <FlexBox height="100%" flexDirection="row" alignItems="flex-start">
                    <FlexBox alignItems="flex-start" justifyContent="flex-start" flex={1} flexDirection="column">
                        <Heading
                            margin="0px"
                            color="primary"
                            fontSize="h3"
                            style={{ cursor: "pointer" }}
                            onClick={() => {
                                window.open("https://github.com/Azure-Samples/azure-search-openai-demo", "_blank", "noreferrer");
                            }}
                        >
                            GitHub Repo: OpenAI Demo
                        </Heading>
                        <Heading
                            margin="0px"
                            color="primary"
                            fontSize="h3"
                            style={{ cursor: "pointer" }}
                            onClick={() => {
                                window.open(
                                    "https://techcommunity.microsoft.com/t5/ai-applied-ai-blog/revolutionize-your-enterprise-data-with-chatgpt-next-gen-apps-w/ba-p/3762087",
                                    "_blank",
                                    "noreferrer"
                                );
                            }}
                        >
                            Blog Post
                        </Heading>
                    </FlexBox>
                </FlexBox>
                <Notes>
                    This is the architecture of the OpenAI demo. It contains links to the GitHub repo for the Open AI demo and the tech community blog post that
                    describes the demo. AI does not work alone. It cannot index the content. Therefore, AI works in collaboration with search. Search indexes
                    the results and AI then formulates a response, using the search results as reference to support its claim.
                    <ol>
                        <li>https://github.com/Azure-Samples/azure-search-openai-demo</li>
                        <li>
                            https://techcommunity.microsoft.com/t5/ai-applied-ai-blog/revolutionize-your-enterprise-data-with-chatgpt-next-gen-apps-w/ba-p/3762087
                        </li>
                    </ol>
                </Notes>
            </Slide>
            <Slide backgroundImage={`url(${Slide12})`} backgroundOpacity={1}>
                <FlexBox height="100%" flexDirection="row" alignItems="flex-end">
                    <FlexBox alignItems="flex-start" justifyContent="flex-start" flex={1} flexDirection="column">
                        <Heading
                            margin="0px"
                            color="primary"
                            fontSize="h4"
                            style={{ cursor: "pointer" }}
                            onClick={() => {
                                window.open("https://learn.microsoft.com/en-us/azure/cognitive-services/openai/concepts/use-your-data", "_blank", "noreferrer");
                            }}
                        >
                            Using your data with Azure OpenAI Service - Azure OpenAI | Microsoft Learn
                        </Heading>
                    </FlexBox>
                </FlexBox>
            </Slide>
            <Slide backgroundColor={theme.colors.primary} backgroundOpacity={1}>
                <FlexBox height="100%" flexDirection="row" alignItems="center">
                    <FlexBox alignItems="flex-start" justifyContent="flex-start" flex={1}>
                        <Heading
                            margin="0px"
                            color="#FFF"
                            fontSize="h2"
                            style={{ cursor: "pointer" }}
                            onClick={() => {
                                window.open(`/${company}`, "_blank", "noreferrer");
                            }}
                        >
                            Demo
                        </Heading>
                    </FlexBox>
                </FlexBox>
            </Slide>
            <Slide backgroundImage={`url(${Slide14})`} backgroundOpacity={1}>
                <FlexBox height="100%" flexDirection="row" alignItems="center">
                    <CodePane language="javascript">
                        {`
                            POST https://[search service].search.windows.net/indexes/securedfiles/docs/index?api-version=2020-06-30
                            {
                                "name": "securedfiles",  
                                "fields": [
                                    {"name": "file_id", "type": "Edm.String", "key": true, "searchable": false },
                                    {"name": "file_name", "type": "Edm.String", "searchable": true },
                                    {"name": "file_description", "type": "Edm.String", "searchable": true },
                                    {"name": "group_ids", "type": "Collection(Edm.String)", "filterable": true, "retrievable": false }
                                ]
                            }`}
                    </CodePane>
                </FlexBox>
            </Slide>
            <Slide backgroundImage={`url(${Slide15})`} backgroundOpacity={1}>
                <FlexBox height="100%" flexDirection="column" alignItems="flex-start" justifyContent="flex-end">
                    <FlexBox alignItems="flex-start" justifyContent="flex-end" flex={1} flexDirection="column">
                        <Heading
                            margin="0px"
                            color="primary"
                            fontSize="h4"
                            style={{ cursor: "pointer" }}
                            onClick={() => {
                                window.open("https://learn.microsoft.com/en-us/azure/search/search-security-trimming-for-azure-search", "_blank", "noreferrer");
                            }}
                        >
                            Security filters for trimming results - Azure Cognitive Search | Microsoft Learn
                        </Heading>
                        <Heading
                            margin="0px"
                            color="primary"
                            fontSize="h4"
                            style={{ cursor: "pointer" }}
                            onClick={() => {
                                window.open(
                                    "https://learn.microsoft.com/en-us/azure/search/search-security-trimming-for-azure-search-with-aad",
                                    "_blank",
                                    "noreferrer"
                                );
                            }}
                        >
                            Security filters to trim results using Active Directory - Azure Cognitive Search | Microsoft Learn
                        </Heading>
                    </FlexBox>
                </FlexBox>
            </Slide>
            <Slide backgroundImage={`url(${Slide16})`} backgroundOpacity={1}>
                <FlexBox height="100%" flexDirection="row" alignItems="center" style={{ marginTop: "35px" }}>
                    <CodePane language="javascript">
                        {`{
                    "value": [
                        {
                            "@search.action": "upload",
                            "file_id": "1",
                            "file_name": "secured_file_a",
                            "file_description": "File access is restricted to the Human Resources.",
                            "group_ids": ["group_id1"]
                        },
                        {
                            "@search.action": "upload",
                            "file_id": "2",
                            "file_name": "secured_file_b",
                            "file_description": "File access is restricted to Human Resources and Recruiting.",
                            "group_ids": ["group_id1", "group_id2"]
                        }
                    ]
                }
                `}
                    </CodePane>
                </FlexBox>
            </Slide>
            <Slide backgroundImage={`url(${Slide17})`} backgroundOpacity={1}>
                <FlexBox height="100%" flexDirection="row" alignItems="center">
                    <CodePane language="javascript">{`
                    POST https://[service name].search.windows.net/indexes/securedfiles/docs/search?api-version=2020-06-30
                    Content-Type: application/json  
                    api-key: [admin or query key]
                    {
                        "filter":"group_ids/any(g:search.in(g, 'group_id1, group_id2'))"  
                     }
                     
                    `}</CodePane>
                </FlexBox>
            </Slide>
        </Deck>
    );
};

export default Presentation;
