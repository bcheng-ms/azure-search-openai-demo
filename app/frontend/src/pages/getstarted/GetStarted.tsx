import React, { useState } from "react";
import { FlexBox } from "spectacle";
import { Link, Label, TextField } from "@fluentui/react";
const GetStarted = () => {
    const [name, setName] = useState("${name}");
    const [company, setCompany] = useState("${company}");
    return (
        <FlexBox height="100%" flexDirection="row" alignItems="center">
            <FlexBox paddingTop={0} flex={1} alignItems="center" flexDirection="column">
                <FlexBox paddingTop={0} flex={1} width="50%" alignItems="flex-start" flexDirection="column">
                    <FlexBox paddingTop={0} flex={1} alignItems="flex-start" flexDirection="column">
                        <Label>What is your name?</Label>
                        <TextField
                            resizable={false}
                            underlined={true}
                            placeholder={"John Doe"}
                            onChange={(_ev: React.FormEvent<HTMLInputElement | HTMLTextAreaElement>, newValue?: string) => {
                                newValue && setName(newValue);
                            }}
                        />
                    </FlexBox>
                    <FlexBox paddingTop={0} flex={1} alignItems="flex-start" flexDirection="column">
                        <Label>What is the company you are presenting to?</Label>
                        <TextField
                            resizable={false}
                            underlined={true}
                            placeholder={"Microsoft"}
                            onChange={(_ev: React.FormEvent<HTMLInputElement | HTMLTextAreaElement>, newValue?: string) => {
                                newValue && setCompany(newValue);
                            }}
                        />
                    </FlexBox>
                    <FlexBox paddingTop={20} flex={1} alignItems="flex-start" flexDirection="column">
                        <Label>Let's get started!!!</Label>
                        <div>
                            <Link href={`/presentation/${name}/${company}`} target="_blank">
                                Presentation (/presentation/{name}/{company})
                            </Link>
                        </div>
                        <div>
                            <Link href={`/${company}`} target="_blank">
                                Demo (/{company})
                            </Link>
                        </div>
                    </FlexBox>
                </FlexBox>
            </FlexBox>
        </FlexBox>
    );
};

export default GetStarted;
