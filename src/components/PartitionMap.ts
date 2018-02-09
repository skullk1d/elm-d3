import * as d3 from 'd3';

// valid data visualizations
export type PartitionShape = 'rect' | 'path';
// valid color mapping
export interface ColorMap {
    [key: string]: string;
}
// d3 hiearchy
export interface PartitionDatum {
    name: string;
    size?: number;
    children?: PartitionDatum[];
    [others: string]: any;
}

export interface PartitionPath {
    d: string;
    transform: string;
    style: string;
}

export interface PartitionRect {
    x: string;
    y: string;
    width: string;
    height: string;
    style: string;
}

export type PartitionNode = PartitionPath | PartitionRect;

// instantiation
export interface PartitionParams {
    // color representation for each kind of data
    colorMap: ColorMap;
    // y values -- points exceeding max range ignored
    data: PartitionDatum[];
    // shape of visual e.g. 'rect' 'arc' ...
    shape: PartitionShape;
    // client rect width
    width: number;
    // client rect height
    height: number;
}

export class PartitionMap {
    // parents passing in data maps should use this value to map a color, otherwise default
    static DATA_NIL = 'empty';
    static DATA_NIL_DEFAULT_COLOR = '#eeeeee';
    static ARC_INNER_RADIUS_SCALE = 1.9;
    static RECT_STROKE = 6;

    private svgSelections: d3.Selection<SVGElement, any, Element, any>[] = [];
    private colorMap: ColorMap;
    private data: PartitionDatum[];
    private shape: PartitionShape;
    private width: number;
    private height: number;

    public viewBox: string;

    constructor(params: PartitionParams) {
        this.colorMap = params.colorMap;
        this.data = params.data;
        this.shape = params.shape;
        this.width = params.width;
        this.height = params.height;

        this.onInit();
    }

    static mapRangeToOffset(index: number, rangeLen: number): number {
        return index / (rangeLen - 1);
    }

    static mapColorToVal(colorMap: ColorMap, val: string): string {
        let color = colorMap[val];

        if (!color) {
            color = PartitionMap.DATA_NIL_DEFAULT_COLOR;
            console.warn(`Color map does not exist for value of type "${val}"`);
        }

        return color;
    }

    static generateNilDataOfSize(size: number): string[] {
        const nilData: string[] = [];

        while (size > nilData.length) {
            nilData.push(PartitionMap.DATA_NIL);
        }

        return nilData;
    }

    onInit() {
        // root html nodes for each dataset
        this.data.forEach(d => {
            const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
            const selection = d3.select(svg);

            // transform raw data into a PartitionDatum
            const transformedDatum = {
                name: 'partition',
                children: d.children
            };

            // set empty color
            if (!this.colorMap[PartitionMap.DATA_NIL]) {
                this.colorMap[PartitionMap.DATA_NIL] = PartitionMap.DATA_NIL_DEFAULT_COLOR;
            }

            // init container
            this.viewBox = `0 0 ${this.width} ${this.height}`;

            // d3 svg contexts
            this.svgSelections.push(selection);

            // draw the shape we wanna
            this.drawShape(this.shape, selection, transformedDatum);
        });
    }

    drawShape(
        shape: PartitionShape,
        selection: d3.Selection<SVGElement, any, Element, any>,
        transformedDatum: PartitionDatum
    ) {
        switch (shape) {
            case 'rect': {
                this.drawRect(selection, transformedDatum);
                break;
            } case 'path': {
                this.drawPath(selection, transformedDatum);
                break;
            } default: {
                this.drawRect(selection, transformedDatum);
                break;
            }
        }
    }

    drawRect(
        selection: d3.Selection<SVGElement, any, Element, any>,
        transformedDatum: PartitionDatum
    ) {
        // define total size of rectangle as full width of container
        const partition = d3.partition()
            .size([this.width, this.height]);
        const root = d3.hierarchy(transformedDatum)
            .sum(d => d.size);

        partition(root);

        selection.selectAll('rect')
            .data(root.descendants())
            .enter()
            .append('rect')
            .attr('x', (d: any) => d && d.x0 || 0)
            .attr('y', 0)
            .attr('width', (d: any) => d && d.x1 - d.x0 || 0)
            .attr('height', PartitionMap.RECT_STROKE)
            .style('fill', d => d && this.colorMap[d.data.name] || PartitionMap.DATA_NIL_DEFAULT_COLOR);

        this.viewBox = `0 0 ${this.width} ${PartitionMap.RECT_STROKE}`;
    }

    drawPath(
        selection: d3.Selection<SVGElement, any, Element, any>,
        transformedDatum: PartitionDatum
    ) {
        // the arcs, whose radii and angles are scaled to a radius bound to the container size
        const radius = this.width / 2;

        // define total size of sunburst as arclength of circle and format our data as d3 hiearchy
        // NOTE half circle sunburst [Math.PI, radius]
        const partition = d3.partition()
            .size([Math.PI * 2, radius]);
        const root = d3.hierarchy(transformedDatum)
            .sum(d => d.size);

        partition(root);

        // define arc using formatted data and draw on context
        const arc = d3.arc()
            .startAngle((d: any) => d && d.x0 || 0)
            .endAngle((d: any) => d && d.x1 || 0)
            .innerRadius((d: any) => d && d.y0 * PartitionMap.ARC_INNER_RADIUS_SCALE || 0)
            .outerRadius((d: any) => d && d.y1 || 0);

        selection.append('path')
            .attr('d', arc);

        selection.selectAll('path')
            .data(root.descendants())
            .enter()
            .append('path')
            .attr('display', d => d.depth ? null : 'none')
            .attr('d', <any>arc)
            .attr('transform', 'rotate(-90)')
            .style('fill', d => d && this.colorMap[d.data.name] || PartitionMap.DATA_NIL_DEFAULT_COLOR);

        // adapt context to fit circle coords including negative quadrants
        this.viewBox = `${-radius} ${-radius} ${this.width} ${this.height}`;
    }

    getPartitions(): { nodes: PartitionNode[], viewBox: string, tag: PartitionShape }[] {
        // return a list of raw <path> attribuets for each partition in this SVG selection
        const partitions = [];

        this.svgSelections.forEach(selection => {
            let elements = selection.selectAll(this.shape).nodes();

            const nodes = (elements.map((el: Element) => {
                return {
                    path: {
                        d: el.getAttribute('d') || '',
                        transform: el.getAttribute('transform') || '',
                        style: el.getAttribute('style') || ''
                    },
                    rect: {
                        x: el.getAttribute('x'),
                        y: el.getAttribute('y'),
                        width: el.getAttribute('width'),
                        height: el.getAttribute('height'),
                        style: el.getAttribute('style') || ''
                    }
                }[this.shape]
            }));
            
            partitions.push({
                nodes,
                viewBox: this.viewBox,
                tag: this.shape
            });
        });

        return partitions;
    }
}
