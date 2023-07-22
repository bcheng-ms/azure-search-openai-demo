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
import Slide13 from "./images/Slide13.jpg";
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
    let { name = "" } = useParams();
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
                    Spectacle supports notes per slide.
                    <ol>
                        <li>Notes can now be HTML markup!</li>
                        <li>Lists can make it easier to make points.</li>
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
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide12})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide13})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide14})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide15})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide16})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
            <Slide backgroundImage={`url(${Slide17})`} backgroundOpacity={1}>
                <div></div>
            </Slide>
        </Deck>
    );
};

export default Presentation;
