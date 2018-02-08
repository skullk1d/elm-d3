import './assets/css/style.scss';
import { PartitionMap } from './components/PartitionMap';

// node `require` type definition
declare function require(name: string);

// app root
const Elm = require('./Main.elm');
const app = Elm.Main.fullscreen({
    api: "/assets/json"
});

// actor model
app.ports.outgoingData.subscribe(msg => {
    switch (msg.tag) {
        case "LogErrorOutside":
            console.error(msg.data);
            break;
        case "GeneratePartitionMap":
            // generate a native SVG node with paths
            const partitionMap = new PartitionMap(msg.data);
            const partitions = partitionMap.getPartitions();

            // send raw path data to elm -- a list of a list of nodes
            app.ports.incomingData.send({
                tag: "GeneratedPartitionMap",
                data: partitions
            });

            break;
        default:
            break;
    }
})