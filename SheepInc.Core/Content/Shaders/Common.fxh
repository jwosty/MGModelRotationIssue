
bool IsNaN(float x)
{
    return !(x < 0.f || x > 0.f || x == 0.f);
}

#ifndef RENDER_DEPTH_ONLY
#define RENDER_DEPTH_ONLY 0
#endif

float4 ColorPixel(float4 c, float depth) {
    
#if RENDER_DEPTH_ONLY
    float4 c2 = c*0.0001;
    return float4(depth, depth, depth, 1) + c2;
#else
    return c;
#endif
}
