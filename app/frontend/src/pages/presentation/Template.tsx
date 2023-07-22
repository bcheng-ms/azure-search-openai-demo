import React from "react";
import { Box, FlexBox, FullScreen, AnimatedProgress } from "spectacle";

type Props = {
    color?: string;
};
export const AzureTemplate = ({ color = "#fff" }: Props) => (
    <FlexBox justifyContent="space-between" position="absolute" bottom={0} width={1}>
        <Box padding="0 1em">
            <FullScreen color={color} />
        </Box>
        <Box padding="1em">
            <AnimatedProgress color={"#000000"} />
        </Box>
    </FlexBox>
);
