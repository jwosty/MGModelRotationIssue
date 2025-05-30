#if OPENGL
    #define SV_POSITION POSITION
    #define VS_SHADERMODEL vs_3_0
    #define PS_SHADERMODEL ps_3_0
#else
    #define VS_SHADERMODEL vs_4_0_level_9_1
    #define PS_SHADERMODEL ps_4_0_level_9_1
#endif

#include "Macros.fxh"
#include "Common.fxh"

DECLARE_TEXTURE(Texture, 0);
DECLARE_TEXTURE(Texture2, 1);
DECLARE_TEXTURE(HudTexture, 2);

BEGIN_CONSTANTS
    float4 DiffuseColor;
    // Value between 0 and 1 specifying how much of the first vs the second texture to use
    float BlendFactor;
    //float Texture1Opacity;
    //float Texture2Opacity;
    //float Texture3Opacity;
MATRIX_CONSTANTS
    float4x4 WorldViewProj;
END_CONSTANTS

struct PixelInput
{
    float4 Position : SV_POSITION;
    float2 TexCoord : TEXCOORD0;
    float2 HudTexCoord : TEXCOORD1;
};

float4 MainPS(PixelInput input) : SV_TARGET0
{
    float4 d1 = SAMPLE_TEXTURE(Texture, input.TexCoord);
    float4 d2 = SAMPLE_TEXTURE(Texture2, input.TexCoord);
    
    float4 fg = lerp(d1, d2, BlendFactor);
    float4 bg = SAMPLE_TEXTURE(HudTexture, input.TexCoord);
    
    // premultiplied alpha blend formula - see https://gamedev.stackexchange.com/a/165383/39958
    return (bg + ((1 - bg.a) * fg))
            * DiffuseColor;
}

technique PostProcessing
{
    pass P0
    {
        PixelShader = compile PS_SHADERMODEL MainPS();
    }
};