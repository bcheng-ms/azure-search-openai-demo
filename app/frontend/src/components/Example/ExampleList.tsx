import { Example } from "./Example";
import { useParams } from "react-router-dom";
import styles from "./Example.module.css";

export type ExampleModel = {
    text: string;
    value: string;
};

interface Props {
    onExampleClicked: (value: string) => void;
}

export const ExampleList = ({ onExampleClicked }: Props) => {
    const { company } = useParams();
    const EXAMPLES: ExampleModel[] = [
        {
            text: `What is included in my ${company} Health Plus plan that is not in standard?`,
            value: `What is included in my ${company} Health Plus plan that is not in standard?`
        },
        { text: "What happens in a performance review?", value: "What happens in a performance review?" },
        { text: "What does a Product Manager do?", value: "What does a Product Manager do?" }
    ];
    return (
        <ul className={styles.examplesNavList}>
            {EXAMPLES.map((x, i) => (
                <li key={i}>
                    <Example text={x.text} value={x.value} onClick={onExampleClicked} />
                </li>
            ))}
        </ul>
    );
};
